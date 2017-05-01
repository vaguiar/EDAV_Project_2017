\name{rjb.test}
\alias{rjb.test}
\title{Test of Normailty - Robust Jarque Bera Test}
\description{
This function performs the robust and classical Jarque-Bera tests of normality.
}
\usage{
rjb.test(x, option = c("RJB", "JB"), 
         crit.values = c("chisq.approximation", "empirical"), N = 0)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{option}{The choice of the test must be "RJB" (default) or "JB".}
  \item{crit.values}{a character string specifying how the critical 
                 values should be obtained, i.e. approximated by the
                 chisq-distribution (default) or empirically.}
  \item{N}{number of Monte Carlo simulations for the empirical critical values}
}
\details{
The test is based on a joint statistic using skewness and kurtosis
coefficients. The Robust Jarque-Bera (RJB) is the robust version of
the Jarque-Bera (JB) test of normality. In particular, RJB utilizes
the robust standard deviation (namely the Average Absolute Deviation
from the Median (MAAD)) to estimate sample kurtosis and skewness
(default option). For more details see Gel and Gastwirth (2006).
Users can also choose to perform the classical Jarque-Bera test (see
Jarque, C. and Bera, A (1980)).
}
\value{
A list with class htest containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\note{Modified from 'jarque.bera.test' (in 'tseries' package).}

\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R. and Gastwirth, J. L. (2008) \emph{A robust modification of 
the Jarque-Bera test of normality}, Economics Letters 99, 30-32.\cr

Jarque, C. and Bera, A. (1980) \emph{Efficient tests for
normality, homoscedasticity and serial independence of regression
residuals}, Economics Letters 6, 255-259.
}
\author{W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{sj.test}}, \code{\link{rqq}}, \code{jarque.bera.test} (in \emph{tseries} package).}
\examples{
## Normally distributed data
x = rnorm(100)
rjb.test(x)

## Sample Output
##
##        Robust Jarque Bera Test
##
## data:  x
## X-squared = 0.962, df = 2, p-value = 0.6182


## Using zuni data
data(zuni)
rjb.test(zuni[,"Revenue"])

##        Robust Jarque Bera Test
##
## data:  zuni[, "Revenue"] 
## X-squared = 54595.63, df = 2, p-value < 2.2e-16


}
\keyword{htest}
