\name{laplace.test}
\alias{laplace.test}
\title{Goodness-of-fit tests for the Laplace distribution}
\description{
 The function returns five goodness-of-fit test statistics for the Laplace distribution. 
The four statistics are: A2 (Anderson-Darling), W2 (Cramer-von Mises), 
U2 (Watson), D (Kolmogorov-Smirnov), and V (Kuiper). By default, NAs are omitted. 
This function requires the VGAM package.
}

\usage{
laplace.test(y)
}

\arguments{
  \item{y}{a numeric vector of data values.}
}
\value{
A list with the following numeric components.
  \item{A2}{the Anderson-Darling statistic.}
  \item{W2}{the Cramer-von Mises statistic.}
  \item{U2}{the Watson statistic.}
  \item{D}{the Kolmogorov-Smirnov statistic.}
  \item{V}{the Kuiper statistic.}
}

\references{

Puig, P. and Stephens, M. A. (2000). \emph{Tests of fit for the Laplace distribution, with 
applications}. Technometrics 42, 417-424.\cr

Stephens, M. A. (1986). \emph{Tests for the Uniform Distribution}, in Goodness-of-Fit 
techniques, eds. R. B. D'Agostino and M. A. Stephens, New York: Marcel Dekker, chapter 8.

}
\author{Kimihiro Noguchi, Yulia R. Gel}
\seealso{\code{plaplace} (in \emph{VGAM} package)}
\examples{

## Differences in flood levels example taken from Puig and Stephens (2000)
library(VGAM)
y<-c(1.96,1.97,3.60,3.80,4.79,5.66,5.76,5.78,6.27,6.30,6.76,7.65,7.84,7.99,8.51,9.18,
10.13,10.24,10.25,10.43,11.45,11.48,11.75,11.81,12.33,12.78,13.06,13.29,13.98,14.18,
14.40,16.22,17.06)
laplace.test(y)$D

## [1] 0.9177726
## The critical value at the 0.05 significance level is approximately 0.906.
## Thus, the null hypothesis would be rejected at the 0.05 level.
## For the tables of critical values, see Stephens (1986) or Puig and Stephens (2000).

}
\keyword{htest}
