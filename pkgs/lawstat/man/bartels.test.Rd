\name{bartels.test}
\alias{bartels.test}
\title{Ranked Version of von Neumann's Ratio Test for Randomness}
\description{
This function performs the Bartels test for randomness which is
based on the ranked version of von Neumann's ratio (RVN). Users can
choose whether to test against two-sided, negative or positive
correlation. \code{NA}s from the data are omitted.
}
\usage{
bartels.test(y, alternative = c("two.sided", "positive.correlated", 
             "negative.correlated"))
}
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of \code{"two.sided"} (default), \code{"negative.correlated"}, or
          \code{"positive.correlated"}.}
}
\value{
  A list with the following components.
  \item{statistic}{the value of the standardized Bartels statistic.}
  \item{parameter}{RVN ratio.}
  \item{p.value}{the \eqn{p}-value for the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
}
\references{
Bartels, R. (1982). The rank version of von Neumann's ratio test for randomness. 
\emph{Journal of the American Statistical Association} 77: 40--46.
}
\author{
  Kimihiro Noguchi, Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao
}
\seealso{\code{\link{runs.test}} }
\examples{
## Simulate 100 observations from an autoregressive model of 
## the first order AR(1)
y = arima.sim(n = 100, list(ar = c(0.5)))

## Test y for randomness
bartels.test(y)

## Sample Output
##
##        Bartels Test - Two sided
## data:  y
## Standardized Bartels Statistic -4.4929, RVN Ratio =
## 1.101, p-value = 7.024e-06
}
\keyword{htest}
\keyword{distribution}
