\name{lnested.test}
\alias{lnested.test}
\title{Test for a monotonic trend in variances}
\description{
 The function performs a test for a monotonic trend in variances. The test statistic
is based on a combination of the finite intersection approach and the classical Levene 
procedure (using the group means), the modified Brown-Forsythe Levene-type procedure 
(using the group medians) or the modified Levene-type procedure (using the group trimmed means). 
More robust versions of the test using the correction factor or structural zero removal method 
are also available. Two options for calculating critical values, namely, approximated
and bootstrapped, are available. By default, NAs from the data are omitted. 
}

\usage{
lnested.test(y, group, location = c("median", "mean", "trim.mean"), 
tail = c("right","left","both"), trim.alpha = 0.25, 
bootstrap = FALSE, num.bootstrap = 1000, 
correction.method = c("none","correction.factor","zero.removal","zero.correction"), 
correlation.method = c("pearson","kendall","spearman"))
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
  \item{num.bootstrap}{number of bootstrap samples to be drawn when bootstrap is set to TRUE; 
           the default value is 1000.}
  \item{correction.method}{procedures to make the ltrend test more robust; the default option is 
           "none"; "correction.factor" applies the correction factor described by O'Brien (1978) and 
           Keyes and Levy (1997); "zero.removal" performs the structural zero removal method by 
           Hines and Hines (2000); "zero.correction" performs a combination of O'Brien's correction factor 
           and the Hines-Hines structural zero removal method (Noguchi and Gel, 2009); note that the options 
           "zero.removal" and "zero.correction" are only applicable when the location is set to "median"; 
           otherwise, "none" is applied.}
  \item{correlation.method}{measures of correlation; the default option is "pearson", the usual correlation 
           coefficient which is equivalent to the t-test; nonparametric measures of correlation such as 
           "kendall" (Kendall's tau) or "spearman" (Spearman's rho) may also be chosen, in which case, two 
           libraries, Hmisc and Kendall, are required.}
}
\value{
A list with the following vector components.
  \item{T}{the statistic and p-value of the test based on the Tippett p-value combination.}
  \item{F}{the statistic and p-value of the test based on the Fisher p-value combination.}
  \item{N}{the statistic and p-value of the test based on the Liptak p-value combination.}
  \item{L}{the statistic and p-value of the test based on the Mudholkar-George p-value combination.}

Each of the vector components contains the following numeric components.
  \item{statistic}{the value of the test statistic expressed in terms of correlation (Pearson, Kendall, or Spearman).}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test performed.}
  \item{data.name}{a character string giving the name of the data.}
  \item{non.bootstrap.statistic}{the statistic of the test without bootstrap method.}
  \item{non.bootstrap.p.value}{the p-value of the test without bootstrap method.}
}

\references{

Boos, D. D. and Brownie, C. (1989). \emph{Bootstrap methods for testing homogeneity 
of variances.} Technometrics 31, 69-82.\cr

Brown, M. B. and Forsythe, A. B. (1974). \emph{Robust tests for equality of 
variances.} Journal of the American Statistical Association 69, 364-367.\cr

Gastwirth, J. L., Gel, Y. R., and Miao, W. (2008). \emph{The Impact of Levene's Test of Equality 
of Variances on Statistical Theory and Practice}. Working paper, 
Department of Statistics, George Washington University.\cr

Hines, W. G. S. and Hines, R. J. O. (2000). \emph{Increased power with modified forms of the 
Levene (med) test for heterogeneity of variance}. Biometrics 56, 451-454.\cr

Hui, W., Gel, Y. R., and Gastwirth, J. L. (2008). \emph{lawstat: an R package for law, public 
policy and biostatistics}. Journal of Statistical Software 28, Issue 3.\cr

Keyes, T. K. and Levy, M. S. (1997). \emph{Analysis of Levenes test under design imbalance}. 
Journal of Educational and Behavioral Statistics 22, 845-858.\cr

Levene, H. (1960). \emph{Robust Tests for Equality of Variances}, in Contributions 
to Probability and Statistics, ed. I. Olkin, Palo Alto, CA: Stanford Univ. Press.\cr

Lim,T.-S., Loh, W.-Y. (1996) \emph{A comparison of tests of equality of variances}
Computational Statistical \& Data Analysis 22, 287-301.\cr

Mudholkar, G. S., McDermott, M. P., and Mudholkar, A. (1995). \emph{Robust finite-intersection 
tests for homogeneity of ordered variances}. 
Journal of Statistical Planning and Inference 43, 185-195.\cr

Noguchi, K. and Gel, Y. R. (2009) \emph{Combination of Levene-type tests and a finite-intersection
method for testing equality of variances against ordered alternatives}. Working paper, 
Department of Statistics and Actuarial Science, University of Waterloo.\cr

O'Brien, R. G. (1978). \emph{Robust techniques for testing heterogeneity of variance effects 
in factorial designs}. Psychometrika 43, 327-344.

}
\author{Kimihiro Noguchi, W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{neuhauser.hothorn.test}}, \code{\link{levene.test}}, \code{\link{ltrend.test}}, 
\code{\link{mma.test}}, \code{\link{robust.mmm.test}}}
\examples{

library(Hmisc)
library(Kendall)
data(pot)
lnested.test(pot[,"obs"], pot[,"type"], location="median", tail="left", 
correction.method="zero.correction")$N

##   lnested test based on the modified Brown-Forsythe Levene-type procedure using the 
##   group medians with modified structural zero removal method and correction factor 
##   (left-tailed with Pearson correlation coefficient)
##
## data:  pot[, "obs"] 
## Test Statistic (N) = 4.905, p-value = 0.0002618

lnested.test(pot[,"obs"], pot[,"type"], location="median", tail="left", 
correction.method="zero.correction",bootstrap=TRUE,num.bootstrap=500)$N

##   bootstrap lnested test based on the modified Brown-Forsythe Levene-type procedure 
##   using the group medians with modified structural zero removal method and correction 
##   factor (left-tailed with Pearson correlation coefficient)
##
## data:  pot[, "obs"] 
## Test Statistic (N) = 4.9936, p-value = 0.000207

}
\keyword{htest}
