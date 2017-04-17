\name{brunner.munzel.test}
\alias{brunner.munzel.test}
\title{The Brunner--Munzel Test for Stochastic Equality}
\description{
This function performs the Brunner--Munzel test for stochastic
equality of two samples, which is also known as the Generalized Wilcoxon
Test. \code{NA}s from the data are omitted.
}
\usage{
brunner.munzel.test(x, y, alternative = c("two.sided", "greater",
"less"), alpha = 0.05)
}
\arguments{
    \item{x}{the numeric vector of data values from the sample 1.}
    \item{y}{the numeric vector of data values from the sample 2.}
    \item{alpha}{significance level, default is 0.05 for 95\% confidence
          interval.}
    \item{alternative}{a character string specifying the alternative
         hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or
            \code{"less"}. User can specify just the initial letter.}
}

\value{
A list containing the following components:
    \item{statistic}{the Brunner--Munzel test statistic.}
    \item{parameter}{the degrees of freedom.}
    \item{conf.int}{the confidence interval.}
    \item{p.value}{the \eqn{p}-value of the test.}
    \item{data.name}{a character string giving the name of the data.}
    \item{estimate}{an estimate of the effect size, i.e., \eqn{P(X < Y) + 0.5 * P(X =Y )}}
}
\references{
Brunner, E. and Munzel, U. (2000). The nonparametric Behrens-Fisher problem: asymptotic theory and a small-sample approximation. \emph{Biometrical Journal} 42: 17--25.

Neubert, K. and Brunner, E. (2007). A Studentized permutation test for the non-parametric Behrens-Fisher problem. \emph{Computational Statistics and Data Analysis} 51: 5192--5204.\cr

Reiczigel, J., Zakarias, I., and Rozsa, L. (2005). A bootstrap test of stochastic equality of two populations. \emph{The American Statistician} 59: 1--6.
}

\author{
Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao
This function was updated with the help of Dr. Ian Fellows
}
\seealso{\code{wilcox.test}, \code{pwilcox}}
\examples{
## Pain score on the third day after surgery for 14 patients under
## the treatment Y and 11 patients under the treatment N
## (see Brunner and Munzel, 2000).

Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)

brunner.munzel.test(Y, N)

##       Brunner-Munzel Test
## data: Y and N
## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##        0.788961
}
\keyword{htest}
\keyword{nonparametric}
