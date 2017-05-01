\name{MexLJ}
\alias{MexLJ}
\docType{data}
\title{
Data from the longjump final in the 1968 Mexico Olympics.
}
\description{
The best longjumps by the 16 finalists in the 1968 Mexico Olympics.  Each athlete jumped up to six times, though the winner of the Gold Medal, Bob Beamon, only jumped twice.
}
\usage{data(MexLJ)}
\format{
  A data frame with 16 observations on the following variable.
  \describe{
    \item{\code{Jump}}{Distance jumped measured in metres}
  }
}
\source{
\url{http://en.wikipedia.org/wiki/Athletics_at_the_1968_Summer_Olympics_-_Men's_long_jump}
}
\examples{
data(MexLJ, package="GDAdata")
with(MexLJ, summary(Jump))
with(MexLJ, hist(Jump,breaks=seq(7.25,9,0.25)))
}
\keyword{datasets}
