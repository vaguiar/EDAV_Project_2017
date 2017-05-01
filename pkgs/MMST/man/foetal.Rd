\name{foetal}
\alias{foetal}
\docType{data}
\title{
MMST FOETAL DATA
}
\description{
cutaneous potential recordings of a pregnant woman, 554, 556, 592
}
\usage{data(foetal)}
\format{
  A data frame with 2500 observations of ECG points. The first variable
  is a simple timestep, the next five channels are measured near the
  fetus (abdominal signals) and the last three channels were placed on
  the mother's thorax (chest).
  \describe{
    \item{\code{timestep}}{a numeric vector}
    \item{\code{ab1}}{a numeric vector}
    \item{\code{ab2}}{a numeric vector}
    \item{\code{ab3}}{a numeric vector}
    \item{\code{ab4}}{a numeric vector}
    \item{\code{ab5}}{a numeric vector}
    \item{\code{th1}}{a numeric vector}
    \item{\code{th2}}{a numeric vector}
    \item{\code{th3}}{a numeric vector}
  }
}
\details{
}
\source{
  \url{http://homes.esat.kuleuven.be/~smc/daisy/daisydata.html}
}
\references{
  A. Izenman (2008), \emph{Modern Multivariate Statistical Techniques},
  Springer

  de Lathauwer, L., de Moor, B., Vandewalle, J. (2000). Fetal
  electrocardiogram extraction by blind source subspace separation, \emph{IEEE
  Transactions on Biomedical Engineering}, \bold{47}, 567-573. \emph{Proceedings of
  the IEEE SP/Athos Workshop on Higher-Order Statistics}, Girona, Spain,
  pp. 134-138.
}
\examples{
}
\keyword{datasets}
