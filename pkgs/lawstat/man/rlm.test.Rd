\name{rlm.test}
\alias{rlm.test}
\title{Robust L1 Moment-Based (RLM) Goodness-of-Fit Test for the Laplace Distribution}
\description{
This function performs the robust test for the Laplace distribution. Two options for calculating 
critical values, namely, approximated with chisq distribution and empirical, are available.
}
\usage{
rlm.test(x, crit.values = c("chisq.approximation", "empirical"), N = 0) 
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{crit.values}{a character string specifying how the critical 
                 values should be obtained, i.e., approximated by the
                 chisq-distribution (default) or empirical.}
  \item{N}{number of Monte Carlo simulations for the empirical critical values}
}
\details{
The test is based on a joint statistic using skewness and kurtosis coefficients. 
In particular, RLM uses the Average Absolute Deviation from the Median (MAAD),
a robust estimate of standard deviation.
}
\value{
A list with class htest containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R. (2009) \emph{Test of fit for a Laplace distribution against 
heavier tailed alternatives}, Working paper.
}
\author{Kimihiro Noguchi, W. Wallace Hui, Yulia R. Gel}
\seealso{\code{\link{sj.test}}, \code{\link{rjb.test}}, \code{\link{rqq}}, \code{jarque.bera.test} 
(in \emph{tseries} package).}
\examples{
## Laplace distributed data
x = rexp(100)-rexp(100)
rlm.test(x)

## Sample Output
##
##        Robust L1 moment-based goodness-of-fit test using a Chi-squared approximated 
##        critical values
##
## data:  x
## Chi-squared statistic = 0.3945, df = 2, p-value = 0.821

}
\keyword{htest}
