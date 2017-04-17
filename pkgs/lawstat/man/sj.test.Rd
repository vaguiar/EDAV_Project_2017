\name{sj.test}
\alias{sj.test}
\title{Test of Normality - SJ Test}
\description{
This function performs the robust directed test of normality which
is based on the ratio of the classical standard deviation \emph{s} to the
robust standard deviation \emph{J} (Average Absolute Deviation from the
Median (MAAD)) of the sample data.
}
\usage{
sj.test(x, crit.values = c("t.approximation", "empirical"), N = 0)
}
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{crit.values}{a character string specifying how the critical 
                 values should be obtained, i.e. approximated by the
                 t-distribution (default) or empirically.}
  \item{N}{number of Monte Carlo simulations for the empirical critical values}
}
\value{
A list with the following numeric components.
  \item{statistic}{the standardized test statistic}
  \item{p.value}{the p-value.}
  \item{parameter}{the ratio of the classical standard deviation S to
    the robust standard deviation J.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R., Miao, W., and Gastwirth, J. L. (2007) \emph{Robust Directed Tests of
Normality Against Heavy Tailed Alternatives}. 
Computational Statistics and Data Analysis 51, 2734-2746.\cr

}
\author{Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{rqq}}, \code{\link{rjb.test}}, \code{jarque.bera.test} (in \emph{tseries} package) }
\examples{
data(bias)
sj.test(bias)

##        Test of Normality - SJ Test
##
## data:  bias 
## Standardized SJ Statistic = 2.5147, ratio of S to J = 1.068, p-value = 0.0216

}
\keyword{htest}
\keyword{ts}
