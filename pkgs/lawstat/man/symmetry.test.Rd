\name{symmetry.test}
\alias{symmetry.test}
\title{Test of Symmetry}

\description{
This function performs test for symmetry about an unknown median. Users can choose between the Cabilio-Masaro test (Cabilio and Masaro, 1996), the Mira test (Mira, 1999), or the MGG test (Miao, Gel, and Gastwirth, 2006); and using asymptotic distribution of respective statistics or a distribution from \eqn{m}-out-of-\eqn{n} bootstrap. Additionally to the general distribution asymmetry, the function allows to test for negative or positive skeweness (see the argument \code{side}). NAs from the data are omitted.}

\usage{
symmetry.test(x, option = c("MGG", "CM", "M"), side = c("both", "left", "right"), 
    boot = TRUE, B = 1000, q = 8/9)
}

\arguments{
  \item{x}{data to be tested for symmetry.}
  \item{option}{test statistic to be applied. Options include statistic by Miao, Gel, and Gastwirth (2006) (default),  Cabilio and Masaro (1996), and by Mira (1999).}
  \item{side}{choice from the three possible alternative hypotheses: general distribution asymmetry (\code{side="both"}, default), left skewness (\code{side="left"}), or right skewness (\code{side="right"}).}
  \item{boot}{logical value indicates whether \eqn{m}-out-of-\eqn{n} bootstrap will be used to obtain critical values (default), or asymptotic distribution of the chosen statistic.}
  \item{B}{number of bootstrap replications to perform (default is 1000).}
  \item{q}{scalar from 0 to 1 to define a set of possible \eqn{m} for the \eqn{m}-out-of-\eqn{n} bootstrap. Default \code{q = 8/9}. Possible \eqn{m} are then set as the values \code{unique(round(n*(q^j))} greater than 4, where \code{n = length(x)} and \code{j = c(0:20)}.}
}

\details{If the bootstrap option is used (\code{boot = TRUE}), a bootstrap distribution is obtained for each candidate subsample size \eqn{m}. Then, a heuristic method (Bickel et al., 1997; Bickel and Sakov, 2008) is used for the choice of optimal \eqn{m}. Particularly, we use the Wasserstein metric (Ruschendorf, 2001) to calculate distances between different bootstrap distributions and select \eqn{m}, which corresponds to the minimal distance.}

\value{
A list of class htest containing the following components:
  \item{method}{name of the method.}
  \item{data.name}{name of the data.}
  \item{statistic}{value of the test statistic.}
  \item{p.value}{\eqn{p}-value of the test.}
  \item{alternative}{alternative hypothesis.}
  \item{estimate}{bootstrap optimal \eqn{m} (given in the output only if bootstrap was used, i.e., \code{boot = TRUE}).}

}
\references{
Bickel, P. J., Gotze, F., and van Zwet, W. R. (1997). Resampling fewer than \emph{n} observations: gains, losses, and remedies for losses. \emph{Statistica Sinica} 7: 1--31.

Bickel, P. J. and Sakov, A. (2008). On the choice of \eqn{m} in the \eqn{m} out of \eqn{n} bootstrap and confidence bounds for extrema. \emph{Statistica Sinica} 18: 967--985.

Cabilio, P. and Masaro, J. (1996). A simple test of symmetry about an unknown median. \emph{The Canadian Journal of Statistics}, 24(3): 349--361. DOI: \href{http://dx.doi.org/10.2307/3315744}{10.2307/3315744}

Lyubchich, V., Wang, X., Heyes, A., and Gel, Y. R. (2016). A distribution-free \emph{m}-out-of-\emph{n} bootstrap approach to testing symmetry about an unknown median. \emph{Computational Statistics and Data Analysis} 104: 1--9. DOI: \href{http://dx.doi.org/10.1016/j.csda.2016.05.004}{10.1016/j.csda.2016.05.004}

Miao, W., Gel, Y. R., and Gastwirth, J. L. (2006). \emph{A new test of symmetry about an unknown median}. In: A. Hsiung, C.-H. Zhang, and Z. Ying (Eds.) Random Walk, Sequential Analysis and Related Topics --- A Festschrift in Honor of Yuan-Shih Chow. World Scientific Publisher, Singapore, pp. 199--214. DOI: \href{http://dx.doi.org/10.1142/9789812772558_0013}{10.1142/9789812772558_0013}

Mira, A. (1999). Distribution-free test for symmetry based on Bonferroni's measure. \emph{Journal of Applied Statistics}, 26(8): 959--972. DOI: \href{http://dx.doi.org/10.1080/02664769921963}{10.1080/02664769921963}

Ruschendorf, L. (2001). \emph{Wasserstein metric}. In: M. Hazewinkel (Ed.) Encyclopedia of Mathematics. Springer, Berlin.
}


\author{Joseph L. Gastwirth, Yulia R. Gel, Wallace Hui, Vyacheslav Lyubchich, Weiwen Miao, Xingyu Wang (in alphabetical order)} 

\examples{
data(zuni)
symmetry.test(zuni[,"Revenue"], boot = FALSE)

##	Symmetry test by Miao, Gel, and Gastwirth (2006)
##
## data:  zuni[, "Revenue"]
## Test statistic = 5.0321, p-value = 4.851e-07
## alternative hypothesis: the distribution is asymmetric.

}
\keyword{htest}
\keyword{distribution}
