\name{rqq}
\alias{rqq}
\title{Test of Normality using RQQ plots}
\description{
This function produces the robust quantile-quantile (RQQ) and
classical quantile-quantile (QQ) plots for graphical assessment of
normality and optionally adds a line, or a QQ line, to the produced
plot. The QQ line may be chosen to be a 45 degree line or to pass
through the first and third quartiles of the data. NAs from the data are omitted. 
Graphical parameters may be given as arguments to 'rqq'.
}
\usage{
rqq(y, plot.it = TRUE, square.it=TRUE, scale = c("MAD", "J", "classical"), 
    location = c("median", "mean"), line.it = FALSE, 
    line.type = c("45 degrees", "QQ"), col.line = 1, lwd = 1, 
    outliers=FALSE, alpha=0.05, ...)
}
\arguments{
  \item{y}{the input data.}
  \item{plot.it}{logical. Should the result be plotted?}
  \item{square.it}{Logical. Should the plot scales be square?. True is the default.}
  \item{scale}{the choice of a scale estimator, i.e. the classical or robust estimate
                   of the standard deviation.}
  \item{location}{the choice of a location estimator, i.e. the mean or median.}
  \item{line.it}{logical. Should the line be plotted? No line is the default.}
  \item{line.type}{If line.it=TRUE, the choice of a line to be plotted, i.e.
                      the 45 degree line or the line passing through the
                      first and third quartiles of the data.}
  \item{col.line}{the color of the line (if plotted).}
  \item{lwd}{the line width (if plotted).}
  \item{outliers}{logical. Should the outliers be listed in the output?}
  \item{alpha}{significance level of outliers. If outliers=TRUE, then all observations
              that are less than the 100*alpha-th standard normal percentile or greater than
              the 100*(1-alpha)-th standard normal percentile will be listed in the output.}
  \item{\dots}{Other parameters from \code{plot}}
}

\details{
An RQQ plot is a modified QQ plot where data are robustly
standardized by the median and robust measure of spread (rather than
mean and classical standard deviation as in the basic QQ plots) and
then are plotted against the expected standard normal order
statistics (see Gel, Miao and Gastwirth, 2005). Under normality, the
plot of the standardized observations should follow the 45 degrees
line, or QQ line. Both the median and robust standard deviation are
significantly less sensitive to outliers than mean and classical
standard deviation and therefore are more preferable in many
practical situations to assess graphically deviations from normality
(if any). We choose median and MAD as a robust measure of location
and spread for our RQQ plots since this standardization typically
provides a clearer graphical diagnostics of normality. In
particular, deviations from the QQ line are usually more noticeable
in RQQ plots in the case of outliers and heavy tails. Users can also
choose to plot the "45 Degrees" line or the "1st and 3rd Quantile"
line. No line is default.
}
\value{
A list with the following numeric components.
  \item{x}{The x coordinates of the points that were/would be plotted}
  \item{y}{The original 'y' vector, i.e., the corresponding y
          coordinates including 'NA's.}
}
\references{
Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
of Tax Assessment Uniformity}, Journal of Statistical Planning
and Inference 6, 1-12.\cr

Gel, Y. R., Miao, W. and Gastwirth, J. L. (2005) \emph{The
Importance of Checking the Assumptions Underlying Statistical
Analysis: Graphical Methods for Assessing Normality}, Jurimetrics
J. 46, 3-29.\cr

Weisberg, S. (2005) \emph{Applied linear regression}, 3rd Ed, John
Wiley \& Sons, Hoboken, N.J.\cr
}
\author{W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{rjb.test}}, \code{\link{sj.test}}, \code{qqnorm}, \code{qqplot}, \code{qqline}}
\examples{
## Simulate 100 observations: using rnorm() for 
## normally distributed data, Y=N(0,1)
y = rnorm(100)
rqq(y)

## Using michigan data
data(michigan)
rqq(michigan)

}
\keyword{robust}
