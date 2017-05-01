\name{j.maad}
\alias{j.maad}
\title{MAAD estimated Robust Standard Deviation}
\description{
This function computes the average absolute deviation from the 
sample median, which is a consistent robust estimate of the 
population standard deviation for normality distribution data.
NAs from the data are omitted.
}
\usage{
j.maad(x)
}
\arguments{
  \item{x}{a numeric vector of data values.}
}
\value{
A list with the following numeric components.
  \item{J}{Robust Standard Deviation J}
}
\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.
}
\author{Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{cd}}, \code{\link{gini.index}}, \code{\link{rqq}}, 
         \code{\link{rjb.test}}, \code{\link{sj.test}}
}
\examples{
## Simulate 100 observations: using rnorm() 
## for normally distributed data, X=N(0,1)
x = rnorm(100)
j.maad(x)

## Sample Output
##
## MAAD estimated J = 0.9194124302405 for data x
}
\keyword{distribution}
