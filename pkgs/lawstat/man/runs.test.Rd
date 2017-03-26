\name{runs.test}
\alias{runs.test}
\title{Runs Test for Randomness}
\description{
This function performs the runs test for randomness. Users can
choose whether to plot the correlation graph or not, and whether to
test against two-sided, negative or positive correlation. NAs from the data are omitted.
}
\usage{
runs.test(y, plot.it = FALSE, alternative = c("two.sided", 
          "positive.correlated", "negative.correlated"))
}
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{plot.it}{logical flag. If 'TRUE', then the graph will be plotted. 
                 If 'FALSE', then it is not plotted.}
  \item{alternative}{a character string specifying the alternative hypothesis,
          must be one of "two.sided" (default), "negative.correlated" or
          "positive.correlated".}
}
\details{
On the graph observations which are less than the sample median are
represented by letter "A" in red color, and observations which are
greater or equal to the sample median are represented by letter "B"
in blue color.
}
\value{
A list with the following components.
  \item{statistic}{the value of the standardized Runs statistic.}
  \item{p.value}{the p-value for the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
}
\references{Mendenhall, W (1982), \emph{Statistics for Management
and Economics}, 4th Ed., 801-807, Duxbury Press, Boston.}
\author{
  Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao
}
\seealso{\code{\link{bartels.test}}
}
\examples{
##Simulate 100 observations from an autoregressive model 
## of the first order (AR(1))
y = arima.sim(n = 100, list(ar = c(0.5)))

##Test y for randomness
runs.test(y)

## Sample Output
##
##        Runs Test - Two sided
## data: y
## Standardized Runs Statistic = -2.8142, p-value = 0.004889
}
\keyword{htest}
\keyword{distribution}
