\name{mma.test}
\alias{mma.test}
\title{Mudholkar-McDermott-Aumont test for ordered variances for normal samples}
\description{
 The function performs a test for a monotonic trend in variances for normal samples. The test statistic
is based on a combination of the finite intersection approach and the classical F (variance ratio) test. 
By default, NAs are omitted.
}

\usage{
mma.test(y,group,tail=c("right","left","both"))
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

Mudholkar, G. S., McDermott, M. P., & Aumont, J. (1993). \emph{Testing homogeneity of ordered variances}.
Metrika 40, 271-281.

}
\author{Kimihiro Noguchi, Yulia R. Gel}
\seealso{\code{\link{neuhauser.hothorn.test}}, \code{\link{levene.test}}, \code{\link{lnested.test}}, 
\code{\link{ltrend.test}}, \code{\link{robust.mmm.test}}}
\examples{

data(pot)
mma.test(pot[,"obs"], pot[,"type"], tail="left")$N

##   Mudholkar et al. (1993) test (left-tailed)
##
## data:  pot[, "obs"] 
## Test Statistic (N) = 9.9429, p-value = 1.028e-12

}
\keyword{htest}
