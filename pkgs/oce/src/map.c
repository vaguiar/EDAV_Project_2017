// assemble_polygons(): construct lat-lon polygons, hopefully to speed up mapImage()
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

//#define DEBUG

/*


   system("R CMD SHLIB map.c") 
   dyn.load("map.so")
   D <- .Call("assemble_polygons", c(0, 1, 2), c(10, 11, 12))
   plot(D$longitude, D$latitude)
   polygon(D$longitude, D$latitude, col=rainbow(4))


   library(oce)
   library(ncdf)
   con <- open.ncdf("/data/oar/levitus/temperature_annual_1deg.nc")
   lon <- get.var.ncdf(con, "lon")
   lat <- get.var.ncdf(con, "lat")
   SST <- get.var.ncdf(con, "t_an")[,,1]
   Tlim <- c(-2, 30)


*/

// macro to index an array
#define ij(i, j) ((i) + (nrow) * (j))

SEXP map_assemble_polygons(SEXP lon, SEXP lat, SEXP z)
{
    PROTECT(lon = AS_NUMERIC(lon));
    double *lonp = REAL(lon);
    PROTECT(lat = AS_NUMERIC(lat));
    double *latp = REAL(lat);
    PROTECT(z = AS_NUMERIC(z));
    double *zp = REAL(z);
    int nlat = length(lat);
    int nlon = length(lon);
    if (nlon < 1) error("must have at least 2 longitudes");
    if (nlat < 1) error("must have at least 2 latitudes");

    // Note that first dimension of z is for y (here, lat) and second for x (here, lon)

    int nrow = INTEGER(GET_DIM(z))[0];
    int ncol = INTEGER(GET_DIM(z))[1];
    if (nlat != ncol) error("mismatch; length(lat)=%d must equal nrow(z)=%d", nlat, ncol);
    if (nlon != nrow) error("mismatch; length(lon)=%d must equal ncol(z)=%d", nlon, nrow);

    int n = nlon * nlat;
    SEXP polylon, polylat, polyz; 
    PROTECT(polylon = allocVector(REALSXP, 5*n));
    PROTECT(polylat = allocVector(REALSXP, 5*n));
    PROTECT(polyz = allocMatrix(REALSXP, nlon, nlat));
    double *polylonp = REAL(polylon), *polylatp = REAL(polylat), *polyzp = REAL(polyz);

    double latstep = 0.5 * fabs(latp[1] - latp[0]);
    double lonstep = 0.5 * fabs(lonp[1] - lonp[0]);
#ifdef DEBUG
    Rprintf("nlon: %d, nlat: %d, latstep: %f, lonstep: %f\n", nlon, nlat, latstep, lonstep);
#endif
    int k = 0, l=0; // indices for points and polygons
    for (int j = 0; j < ncol; j++) {
        for (int i = 0; i < nrow; i++) {
#ifdef DEBUG
            if (j == 0 && i < 3)
                Rprintf("i: %d, j: %d, lon: %.4f, lat:%.4f, k: %d\n", i, j, lonp[i], latp[j], k);
#endif
            // Lower left
            polylonp[k] = lonp[i] - lonstep;
            polylatp[k++] = latp[j] - latstep;
            // Upper left
            polylonp[k] = lonp[i] - lonstep;
            polylatp[k++] = latp[j] + latstep;
            // Upper right
            polylonp[k] = lonp[i] + lonstep;
            polylatp[k++] = latp[j] + latstep;
            // Lower right
            polylonp[k] = lonp[i] + lonstep;
            polylatp[k++] = latp[j] - latstep;
            // end
            polylonp[k] = NA_REAL;
            polylatp[k++] = NA_REAL;
            polyzp[l++] = zp[ij(i, j)];
#ifdef DEBUG
            if (j == 0 && i < 3)
                for (int kk=k-5; kk<k-1; kk++)
                    Rprintf("k: %d, lon: %.4f, lat:%.4f\n", kk, polylonp[kk], polylatp[kk]);
#endif
        }
        if (k > 5 * n)
            error("coding error (assigned insufficient memory); k: %d,  5*n: %d", k, 5*n);
    }
    if (k != 5 * n)
        error("coding error (assigned surplus memory); k: %d,  5*n: %d", k, 5*n);
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 3));
    PROTECT(res_names = allocVector(STRSXP, 3));
    SET_VECTOR_ELT(res, 0, polylon);
    SET_STRING_ELT(res_names, 0, mkChar("longitude"));
    SET_VECTOR_ELT(res, 1, polylat);
    SET_STRING_ELT(res_names, 1, mkChar("latitude"));
    SET_VECTOR_ELT(res, 2, polyz);
    SET_STRING_ELT(res_names, 2, mkChar("z"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(8);
    return(res);
}


SEXP map_check_polygons(SEXP x, SEXP y, SEXP z, SEXP xokspan, SEXP usr) // returns new x vector
{
    //int nrow = INTEGER(GET_DIM(z))[0];
    //int ncol = INTEGER(GET_DIM(z))[1];
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(z = AS_NUMERIC(z));
    PROTECT(xokspan = AS_NUMERIC(xokspan));
    PROTECT(usr = AS_NUMERIC(usr));
    int nusr = LENGTH(usr);
    if (nusr != 4) error("'usr' must hold 4 values");
    double *usrp = REAL(usr); // left right bottom top
    double *xp = REAL(x);
    double *yp = REAL(y);
    //double *zp = REAL(z);
    double *xokspanp = REAL(xokspan);
    int nx = length(x);
    int ny = length(y);
    int nz = length(z);
    if (nx < 2) error("must have at least two x values");
    if (ny < 2) error("must have at least two y values");
    if (nz < 1) error("must have at least one z value");
    int npoly = nx / 5;

    SEXP okPoint, okPolygon, clippedPoint, clippedPolygon;
    PROTECT(okPolygon = allocVector(LGLSXP, npoly)); 
    PROTECT(okPoint = allocVector(LGLSXP, nx)); 
    PROTECT(clippedPoint = allocVector(LGLSXP, nx)); 
    PROTECT(clippedPolygon = allocVector(LGLSXP, npoly)); 

    int *okPointp = INTEGER(okPoint);
    int *okPolygonp = INTEGER(okPolygon);
    int *clippedPointp = INTEGER(clippedPoint);
    int *clippedPolygonp = INTEGER(clippedPolygon);
    // Initialize (not be needed if below catches all cases)
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        okPolygonp[ipoly] = 1;
        clippedPolygonp[ipoly] = 0;
    }
    for (int ix = 0; ix < nx; ix++) {
        okPointp[ix] = 1;
        clippedPointp[ix] = 0;
    }
    // x1 x2 x3 x4 NA x1 x2 x3 x4 NA ...
    double dxPermitted = fabs(*xokspanp);
#ifdef DEBUG
    int count = 0, ncount=100000;
#endif
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        int start = 5 * ipoly;
        // Check for bad polygons, in three phases.
        // 1. Find polygons that have some NA values for vertices
#ifdef DEBUG
        if (ipoly < 3)
            Rprintf("start: %d; okPointp= %d %d ...\n", start, okPointp[start], okPointp[start+1]);
#endif
        for (int j = 0; j < 4; j++) { // skip 5th point which is surely NA
            // Check for x or y being NA
            if (ISNA(xp[start + j]) || ISNA(yp[start + j])) {
#ifdef DEBUG
                if (count++ < ncount) { // FIXME: remove when working
                    Rprintf("(1.) x or y is NA -- ipoly: %d, j: %d, span: %f (limit to span: %f)\n",
                            ipoly, j, fabs(xp[start+j]-xp[start+j-1]), dxPermitted);
                }
#endif
                for (int k = 0; k < 5; k++)
                    okPointp[start + k] = 0;
                okPolygonp[ipoly] = 0;
                break;
            }
        }
        // 2. Find polygons with all vertices outside the plot region
        double xmin = xp[start], xmax = xp[start], ymin = yp[start], ymax=yp[start];
        for (int j = 1; j < 4; j++) {
            if (xp[start + j] < xmin) xmin = xp[start + j];
            if (yp[start + j] < ymin) ymin = yp[start + j];
            if (xp[start + j] > xmax) xmax = xp[start + j];
            if (yp[start + j] > ymax) ymax = yp[start + j];
        }
        if (xmax < usrp[0] || usrp[1] < xmin || ymax < usrp[2] || usrp[3] < ymin) {
#ifdef DEBUG
            if (count < ncount) {
                count++;
                Rprintf("clipping points %d to %d\n", start, start+4);
            }
#endif
            for (int k = 0; k < 5; k++) {
                clippedPointp[start + k] = 1;
            }
            clippedPolygonp[ipoly] = 1;
        }
        // 3. Find polygons with excessive x range (an error in projection)
        for (int j = 1; j < 4; j++) { // skip 5th point which is surely NA
            if (dxPermitted < fabs(xp[start + j] - xp[start + j - 1])) {
#ifdef DEBUG
                if (count++ < ncount) { // FIXME: remove when working
                    Rprintf("(3.) ipoly: %d, j: %d, span: %f (limit to span: %f)\n",
                            ipoly, j, fabs(xp[start+j]-xp[start+j-1]), dxPermitted);
                }
#endif
                for (int k = 0; k < 5; k++) {
                    okPointp[start + k] = 0;
                }
                okPolygonp[ipoly] = 0;
                break;
            }
        }
    }
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 4));
    PROTECT(res_names = allocVector(STRSXP, 4));
    SET_VECTOR_ELT(res, 0, okPoint);
    SET_STRING_ELT(res_names, 0, mkChar("okPoint"));
    SET_VECTOR_ELT(res, 1, clippedPoint);
    SET_STRING_ELT(res_names, 1, mkChar("clippedPoint"));
    SET_VECTOR_ELT(res, 2, okPolygon);
    SET_STRING_ELT(res_names, 2, mkChar("okPolygon"));
    SET_VECTOR_ELT(res, 3, clippedPolygon);
    SET_STRING_ELT(res_names, 3, mkChar("clippedPolygon"));
    setAttrib(res, R_NamesSymbol, res_names);

    UNPROTECT(11);
    return(res);
#undef ij
}

