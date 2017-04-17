\name{levene.test}
\alias{levene.test}
\title{Levene's Test of Equality of Variances}
\description{
The function performs the following tests for equality of the k population variances:
classical Levene's test, the robust Brown-Forsythe Levene-type 
test using the group medians and the robust Levene-type test using 
the group trimmed mean. More robust versions of the test using the correction factor or 
structural zero removal method are also available. Two options for calculating critical values, namely, 
approximated and bootstrapped, are available. Instead of the ANOVA statistic suggested by Levene, 
the Kruskal-Wallis ANOVA may also be applied  using this function. 
By default, NAs from the data are omitted. 
}
\usage{
levene.test(y, group, location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
bootstrap = FALSE, num.bootstrap=1000, kruskal.test=FALSE, 
correction.method=c("none","correction.factor","zero.removal","zero.correction"))
}
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{factor of the data.}
  \item{location}{the default option is "median" corresponding to the robust Brown-Forsythe 
           Levene-type procedure; "mean" corresponds to the classical Levene's procedure, 
           and "trim.mean" corresponds to the robust Levene-type procedure using the group 
           trimmed means.}
  \item{trim.alpha}{the fraction (0 to 0.5) of observations to be trimmed from
          each end of 'x' before the mean is computed.}
  \item{bootstrap}{the default option is FALSE, i.e., no bootstrap; if the option is set to 
           TRUE, the function performs the bootstrap method described in Lim and Loh (1996) 
           for Levene's test.}
  \item{num.bootstrap}{number of bootstrap samples to be drawn when the bootstrap option is set to TRUE; 
           the default value is 1000.}
  \item{kruskal.test}{use of Kruskal-Wallis statistic. The default option is FALSE, i.e., the usual 
           ANOVA statistic is used in place of Kruskal-Wallis statistic. }
  \item{correction.method}{procedures to make the levene's test more robust; the default option is 
           "none"; "correction.factor" applies the correction factor described by O'Brien (1978) and 
           Keyes and Levy (1997); "zero.removal" performs the structural zero removal method by 
           Hines and Hines (2000); "zero.correction" performs a combination of O'Brien's correction factor 
           and the Hines-Hines structural zero removal method (Noguchi and Gel, 2009); note that the options 
           "zero.removal" and "zero.correction" are only applicable when the location is set to "median"; 
           otherwise, "none" is applied.}
}
\details{
  Levene (1960) proposed a test for homogeneity of variances in k groups which
is based on the ANOVA statistic applied to absolute deviations of observations 
from the corresponding group mean. The robust Brown-Forsythe version
of the Levene-type test substites the group mean by the group median in the classical 
Levene statistic. The third option is to consider ANOVA applied to the 
absolute deviations of observations from the group trimmed mean instead of 
the group means.
}
\value{
A list with the following numeric components.
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test performed.}
  \item{data.name}{a character string giving the name of the data.}
  \item{non.bootstrap.p.value}{the p-value of the test without bootstrap method; i.e. the p-value 
	using the approximated critical value.}
}

\note{modified from a response posted by Brian Ripley to the R-help e-mail list.}

\references{

Boos, D. D. and Brownie, C. (1989). \emph{Bootstrap methods for testing homogeneity 
of variances.} Technometrics 31, 69-82.\cr

Brown, M. B. and Forsythe, A.B. (1974). \emph{Robust tests for equality of 
variances.} Journal of the American Statistical Association, 69, 364-367.\cr

Gastwirth, J. L., Gel, Y. R., and Miao, W. (2009). \emph{The Impact
of Levene's Test of Equality of Variances on Statistical Theory and
Practice}. Statistical Science, 24(3), 343-360. \cr

Hines, W. G. S. and Hines, R. J. O. (2000). \emph{Increased power with modified forms of the 
Levene (med) test for heterogeneity of variance}. Biometrics 56, 451-454.\cr

Hui, W., Gel, Y. R., and Gastwirth, J. L. (2008). \emph{lawstat: an R package for law, public 
policy and biostatistics}. Journal of Statistical Software 28, Issue 3.\cr

Keyes, T. K. and Levy, M. S. (1997). \emph{Analysis of Levenes test under design imbalance}. 
Journal of Educational and Behavioral Statistics 22, 845-858.\cr

Kruskal, W. H. and Wallis, W. A. (1952). \emph{Use of ranks in one-criterion variance analysis}. 
Journal of the American Statistical Association 47, 583-621.\cr

Levene, H. (1960). \emph{Robust Tests for Equality of Variances}, in Contributions 
to Probability and Statistics, ed. I. Olkin, Palo Alto, CA: Stanford Univ. Press.\cr

Lim,T.-S., Loh, W.-Y. (1996) \emph{A comparison of tests of equality of variances}
Computational Statistical \& Data Analysis 22, 287-301.\cr

Noguchi, K. and Gel, Y. R. (2009) \emph{Combination of Levene-type tests and a finite-intersection
method for testing equality of variances against ordered alternatives}. Working paper, 
Department of Statistics and Actuarial Science, University of Waterloo.\cr

O'Brien, R. G. (1978). \emph{Robust techniques for testing heterogeneity of variance effects 
in factorial designs}. Psychometrika 43, 327-344.

}


\author{Kimihiro Noguchi, W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{neuhauser.hothorn.test}}, \code{\link{lnested.test}}, \code{\link{ltrend.test}}, 
\code{\link{mma.test}}, \code{\link{robust.mmm.test}}}
\examples{

data(pot)
levene.test(pot[,"obs"], pot[,"type"], location="median", correction.method="zero.correction")

##        modified robust Brown-Forsythe Levene-type test based on the absolute deviations 
##        from the median with modified structural zero removal method and correction factor
##
## data:  pot[,"obs"] 
## Test Statistic = 6.5673, p-value = 0.001591

##   Bootstrap version of the test. The calculation may take up a few minutes 
##   depending on the number of bootstrap sampling.

levene.test(pot[,"obs"], pot[,"type"], location="median", correction.method="zero.correction", 
bootstrap=TRUE,num.bootstrap=500)

##        bootstrap modified robust Brown-Forsythe Levene-type test based on the absolute 
##        deviations from the median with structural zero removal method and correction factor
##
## data:  pot[, "obs"] 
## Test Statistic = 6.9577, p-value = 0.001

}
\keyword{htest}
