\name{neuhauser.hothorn.test}
\alias{neuhauser.hothorn.test}
\title{Neuhauser-Hothorn double contrast test for a monotonic trend in variances}
\description{
 The function performs a test for a monotonic trend in variances. The test statistic
suggested by Neuhauser and Hothorn (2000) is based on the classical Levene procedure 
(using the group means), the modified Brown-Forsythe Levene-type procedure 
(using the group medians) or the modified Levene-type procedure (using the group trimmed means). 
More robust versions of the test using the correction factor or structural zero removal method 
are also available. Two options for calculating critical values, namely, approximated
and bootstrapped, are available. By default, NAs from the data are omitted. 
This function requires the mvtnorm package.
}

\usage{
neuhauser.hothorn.test(y, group, location = c("median", "mean", "trim.mean"), 
tail = c("right","left","both"), trim.alpha = 0.25, 
bootstrap = FALSE, num.bootstrap = 1000, 
correction.method = c("none","correction.factor","zero.removal","zero.correction"))
}

\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{factor of the data.}
  \item{location}{the default option is "median" corresponding to the robust Brown-Forsythe 
           Levene-type procedure; "mean" corresponds to the classical Levene's procedure, 
           and "trim.mean" corresponds to the robust Levene-type procedure using the group 
           trimmed means.}
  \item{tail}{the default option is "right", corresponding to an increasing trend in variances 
           as the one-sided alternatives; "left" corresponds to a decreasing trend in variances, 
           and "both" corresponds to any (increasing or decreasing) monotonic trend in variances
           as the two-sided alternatves.}
  \item{trim.alpha}{the fraction (0 to 0.5) of observations to be trimmed from
           each end of 'x' before the mean is computed.}
  \item{bootstrap}{the default option is FALSE, i.e., no bootstrap; if the option is set to 
           TRUE, the function performs the bootstrap method described in Lim and Loh (1996) 
           for Levene's test.}
  \item{num.bootstrap}{number of bootstrap samples to be drawn when the bootstrap option is set to TRUE; 
           the default value is 1000.}
  \item{correction.method}{procedures to make the ltrend test more robust; the default option is 
           "none"; "correction.factor" applies the correction factor described by O'Brien (1978) and 
           Keyes and Levy (1997); "zero.removal" performs the structural zero removal method by 
           Hines and Hines (2000); "zero.correction" performs a combination of O'Brien's correction factor 
           and the Hines-Hines structural zero removal method (Noguchi and Gel, 2009); note that the options 
           "zero.removal" and "zero.correction" are only applicable when the location is set to "median"; 
           otherwise, "none" is applied.}
}

\value{
A list with the following numeric components.
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test performed.}
  \item{data.name}{a character string giving the name of the data.}
  \item{non.bootstrap.p.value}{the p-value of the test without bootstrap method.}
}
\references{

Boos, D. D. and Brownie, C. (1989). \emph{Bootstrap methods for testing homogeneity 
of variances.} Technometrics 31, 69-82.\cr

Brown, M. B. and Forsythe, A. B. (1974). \emph{Robust tests for equality of 
variances.} Journal of the American Statistical Association, 69, 364-367.\cr

Hines, W. G. S. and Hines, R. J. O. (2000). \emph{Increased power with modified forms of the 
Levene (med) test for heterogeneity of variance}. Biometrics 56, 451-454.\cr

Keyes, T. K. and Levy, M. S. (1997). \emph{Analysis of Levenes test under design imbalance}. 
Journal of Educational and Behavioral Statistics 22, 845-858.\cr

Levene, H. (1960). \emph{Robust Tests for Equality of Variances}, in Contributions 
to Probability and Statistics, ed. I. Olkin, Palo Alto, CA: Stanford Univ. Press.\cr

Neuhauser, M. and Hothorn, L. A. (2000). \emph{Location-scale and scale trend tests based on 
Levene's transformation}. Computational Statistics and Data Analysis 33, 189-200.\cr

Noguchi, K. and Gel, Y. R. (2009) \emph{Combination of Levene-type tests and a finite-intersection
method for testing equality of variances against ordered alternatives}. Working paper, 
Department of Statistics and Actuarial Science, University of Waterloo.\cr

O'Brien, R. G. (1978). \emph{Robust techniques for testing heterogeneity of variance effects 
in factorial designs}. Psychometrika 43, 327-344.

}
\author{Kimihiro Noguchi, Yulia R. Gel}
\seealso{\code{\link{levene.test}}, \code{\link{lnested.test}}, \code{\link{ltrend.test}}, 
\code{\link{mma.test}}, \code{\link{robust.mmm.test}}}
\examples{

library(mvtnorm)
data(pot)
neuhauser.hothorn.test(pot[,"obs"], pot[,"type"], location="median", tail="left", 
correction.method="zero.correction")

##   double contrast test based on the absolute deviations from the median with 
##   group medians with modified structural zero removal method and correction factor 
##   (left-tailed)
##
## data:  pot[, "obs"] 
## Test Statistic = -3.6051, p-value = 0.0003021

##   Bootstrap version of the test. The calculation may take up a few minutes 
##   depending on the number of bootstrap sampling.

neuhauser.hothorn.test(pot[,"obs"], pot[,"type"], location="median", tail="left", 
correction.method="zero.correction", bootstrap=TRUE, num.bootstrap=500)

##   bootstrap double contrast test based on the absolute deviations from the median with 
##   modified structural zero removal method and correction factor 
##   (left-tailed)
##
## data:  pot[, "obs"] 
## Test Statistic = -3.6051, p-value = 0.0001

}
\keyword{htest}
