\name{SpeedSki}
\alias{SpeedSki}
\docType{data}
\title{
World Speed Skiing Competition, Verbier 21st April, 2011
}
\description{
There were separate Speed Skiing competitions for men (79 participants) and women (12 participants).
}
\usage{data(SpeedSki)}
\format{
  A data frame with 91 observations on the following 8 variables.
  \describe{
    \item{\code{Rank}}{Finishing position by sex}
    \item{\code{Bib}}{Start number}
    \item{\code{FIS.Code}}{Skier's international skiing ID number}
    \item{\code{Name}}{Skier's name}
    \item{\code{Year}}{Skier's year of birth}
    \item{\code{Nation}}{Skier's nationality}
    \item{\code{Speed}}{Speed achieved in km/hr}
    \item{\code{Sex}}{Female or Male}
    \item{\code{Event}}{Speed Downhill, Speed Downhill Junior or Speed One}
    \item{\code{no.of.runs}}{No of runs}
  }
}
\source{
\url{http://www.fis-ski.com/de/606/612.html?sector=SS&raceid=262}  (men) \cr
\url{http://www.fis-ski.com/de/606/612.html?sector=SS&raceid=263}  (women)
}
\examples{
data(SpeedSki, package="GDAdata")
with(SpeedSki, summary(Speed))
library(ggplot2)
ggplot(SpeedSki, aes(Speed)) + geom_histogram(binwidth=5)
}
\keyword{datasets}
