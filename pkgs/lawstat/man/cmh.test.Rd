\name{cmh.test}
\alias{cmh.test}
\title{The Cochran-Mantel-Haenszel Chi-square Test}
\description{
This function performs the Cochran-Mantel-Haenszel (CMH) procedure. The CMH procedure 
tests homogeneity of population proportions after taking into account other factors. 
This procedure is widely used in various law cases, in particular, on equal employment 
and discrimination, as well in biological and phamaceutical studies.
}

\usage{
cmh.test(x)
}
\arguments{
  \item{x}{a numeric 2 x 2 x k array of data values.}
}

\details{
The test is based on the CMH procedure discussed by Gastwirth, 1984. The data should be input
in a array of 2 rows x 2 columns x k levels. The output includes the Mantel-Haenszel Estimate,
the pooled Odd Ratio, and the Odd Ratio between the rows and columns at each level. The Chi-square
Test of Significance tests if there is an interaction or association between rows and columns. 

The null hypothesis is that the pooled Odd Ratio is equal to 1, i.e., there is no interaction between
rows and columns. For more details see Gastwirth (1984).

Notice that cmh.test can be viewed as a subset of mantelhaen.test, in
the sense that cmh.test is for a 2 by 2 by k table without continuity
correction whereas mantelhaen.test allows for a larger table, and for a 2 by 2
by k table, it has an option of performing continuity correction or not.
However, in view of Gastwirth (1984), continuity correction is not
recommended as it tends to overestimate the p-value.
}


\value{
A list with class htest containing the following components:
  \item{MH.ESTIMATE}{the value of the Cochran-Mantel-Haenszel Estimate.}
  \item{OR}{Pooled Odd Ratio of the data.}
  \item{ORK}{vector of Odd Ratio of each level}
  \item{cmh}{the test statistic.}
  \item{df}{degrees of freedom.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Gastwirth, J. L.(1984) \emph{Statistical Methods for Analyzing Claims of Employment Discrimination}, Industrial and Labor Relations Review, Vol. 38, No. 1. (October 1984), pp. 75-86.\cr
}

\author{Min Qin, Wallace W. Hui, Yulia R. Gel, Joseph L. Gastwirth}

\seealso{\code{mantelhaen.test}}

\examples{

## Sample Salary Data

data(blackhire)
cmh.test(blackhire)


## Sample Output
##
##        Mantel-Haenszel Chi-square Test
##
## data:  blackhire 
## Mantel-Haenszel Estimate = 0.477, Chi-squared = 145.840, df = 1.000, p-value = 0.000,
## Pooled Odd Ratio = 0.639, Odd Ratio of level 1 = 1.329, Odd Ratio of level 2 = 0.378, Odd
## Ratio of level 3 = 0.508, Odd Ratio of level 4 = 0.357, Odd Ratio of level 5 = 0.209, Odd
## Ratio of level 6 = 0.412, Odd Ratio of level 7 = 0.250, Odd Ratio of level 8 = 0.820
## 
## Note: P-value is significant and we should reject the null hypothesis.

}
\keyword{htest}

