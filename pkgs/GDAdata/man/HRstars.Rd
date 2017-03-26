\name{HRstars}
\alias{HRstars}
\docType{data}
\title{
Star data useful for drawing a Hertzsprung-Russell diagram.
}
\description{
Hertzsprung-Russell diagrams plot star luminosity (brightness) against temperature (colour).  The first one was drawn just over 100 years ago.  The dataset is the Yale Trigonometric Parallax Dataset and this version can be found on the webpage of the Astronomy Department of Case Western Reserve University.
}
\usage{data(HRstars)}
\format{
  A data frame with 6220 observations on the following 5 variables.
  \describe{
    \item{\code{ID}}{star ID number}
    \item{\code{V}}{apparent V magnitude}
    \item{\code{BV}}{observed B-V color}
    \item{\code{Para}}{observed parallax (in arcsec)}
    \item{\code{Uncert}}{uncertainty in parallax (in milliarcsec)}
    }
}
\source{
\url{http://burro.astr.cwru.edu/Academics/Astr221/HW/HW5/HW5.html}
}
\examples{
data(HRstars, package="GDAdata")
with(HRstars, hist(BV))
with(HRstars, hist(V))
}
\keyword{datasets}
