\name{nig.parameter}
\alias{nig.parameter}
\title{Generating parameters for the normal inverse Gaussian (NIG) distribution}
\description{
 The function produces four parameters, alpha (tail heavyness), beta (asymmetry), 
delta (scale), and mu (location) from the four variables, mean, variance, 
kurtosis, and skewness. 
}

\usage{
nig.parameter(mean=mean, variance=variance, kurtosis=kurtosis, skewness=skewness)
}

\arguments{
  \item{mean}{mean of the NIG distribution.}
  \item{variance}{variance of the NIG distribution.}
  \item{kurtosis}{excess kurtosis of the NIG distribution.}
  \item{skewness}{skewness of the NIG distribution.}
}

\details{
 The parameters are generated on three conditions: 1. $3*kurtosis > 5*skewness^2$, 2. $skewness > 0$, 
and 3. $variance > 0$.
}

\value{
A list with the following numeric components.
  \item{alpha}{tail-heavyness parameter of the NIG distribution.}
  \item{beta}{asymmetry parameter of the NIG distribution.}
  \item{delta}{scale parameter of the NIG distribution.}
  \item{mu}{location parameter of the NIG distribution.}
}

\references{

Atkinson, A. C. (1982). \emph{The simulation of generalized inverse Gaussian and hyperbolic random variables}. 
SIAM Journal on Scientific and Statistical Computing 3, 502-515.\cr

Barndorff-Nielsen O., Blaesild, P. (1983). \emph{Hyperbolic distributions}. In Encyclopedia of Statistical 
Sciences, Eds., Johnson N.L., Kotz S. and Read C.B., Vol. 3, pp. 700-707. New York: Wiley.\cr 

Noguchi, K. and Gel, Y. R. (2009) \emph{Combination of Levene-type tests and a finite-intersection
method for testing equality of variances against ordered alternatives}. Working paper, 
Department of Statistics and Actuarial Science, University of Waterloo.

}
\author{Kimihiro Noguchi, Yulia R. Gel}
\seealso{\code{rnig} (in \emph{fBasics} package)}
\examples{

library(fBasics)
test<-nig.parameter(0,2,5,1)
random<-rnig(1000000,alpha=test$alpha,beta=test$beta,mu=test$mu,delta=test$delta)
mean(random)
##   [1] 0.0003896483
var(random)
##   [1] 2.007351
kurtosis(random)
##   [1] 5.085051
##   attr(,"method")
##   [1] "excess"
skewness(random)
##   [1] 1.011352
##   attr(,"method")
##   [1] "moment"

}
\keyword{htest}
