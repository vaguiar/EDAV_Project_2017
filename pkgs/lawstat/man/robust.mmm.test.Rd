\name{robust.mmm.test}
\alias{robust.mmm.test}
\title{Robust Mudholkar-McDermott-Mudholkar test for ordered variances}
\description{
 The function performs a test for a monotonic trend in variances. The test statistic is based on 
a combination of the finite intersection approach and the two-sample t-test using Miller's 
transformation. By default, NAs are omitted.
}

\usage{
robust.mmm.test(y,group,tail=c("right","left","both"))
}

\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{factor of the data.}
  \item{tail}{the default option is "right", corresponding to an increasing trend in variances 
           as the one-sided alternatives; "left" corresponds to a decreasing trend in variances, 
           and "both" corresponds to any (increasing or decreasing) monotonic trend in variances
           as the two-sided alternatves.}
}
\value{
A list with the following vector components.
  \item{T}{the statistic and p-value of the test based on the Tippett p-value combination.}
  \item{F}{the statistic and p-value of the test based on the Fisher p-value combination.}
  \item{N}{the statistic and p-value of the test based on the Liptak p-value combination.}
  \item{L}{the statistic and p-value of the test based on the Mudholkar-George p-value combination.}

Each of the vector components contains the following numeric components.
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\references{

Mudholkar, G. S., McDermott, M. P., & Mudholkar, A. (1995). \emph{Robust finite-intersection tests 
for homogeneity of ordered variances}.
Journal of Statistical Planning and Inference 43, 185-195.

}
\author{Kimihiro Noguchi, Yulia R. Gel}
\seealso{\code{\link{neuhauser.hothorn.test}}, \code{\link{levene.test}}, \code{\link{lnested.test}}, 
\code{\link{ltrend.test}}, \code{\link{mma.test}}}
\examples{

data(pot)
robust.mmm.test(pot[,"obs"], pot[,"type"], tail="left")$N

##   Mudholkar et al. (1995) test (left-tailed)
##
## data:  pot[, "obs"] 
## Test Statistic (N) = 7.4079, p-value = 8.109e-08

}
\keyword{htest}
