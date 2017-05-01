\name{Decathlon}
\alias{Decathlon}
\docType{data}
\title{
Top performances in the Decathlon from 1985 to 2006.
}
\description{
The point scoring system for the Decathlon last changed in 1985.  Best annual performances of 6800 points and over for a twenty-one year period after the new rules were introduced were downloaded from the excellent Estonian website Decathlon2000.  Handtimed performances were not included.  Names with accents have been simplified.
}
\usage{data(Decathlon)}
\format{
  A data frame with 7968 observations on the following 24 variables.
  \describe{
    \item{\code{Totalpoints}}{the total points achieved over all 10 events}
    \item{\code{DecathleteName}}{Decathlete's name}
    \item{\code{Nationality}}{Decathlete's nationality}
    \item{\code{m100}}{Time for the 100 metres (secs)}
    \item{\code{Longjump}}{Distance jumped (metres)}
    \item{\code{Shotput}}{Distance putting the shot (metres)}
    \item{\code{Highjump}}{Height jumped (metres)}
    \item{\code{m400}}{Time for the 400 metres (secs)}
    \item{\code{m110hurdles}}{Time for the 110 metres hurdles (secs)}
    \item{\code{Discus}}{Distance throwing the discus (metres)}
    \item{\code{Polevault}}{Height achieved (metres)}
    \item{\code{Javelin}}{Distance throwing the javelin (metres)}
    \item{\code{m1500}}{Time for the 1500 metres (secs)}
    \item{\code{yearEvent}}{Year of performance}
    \item{\code{P100m}}{Points for performance in 100 metres}
    \item{\code{Plj}}{Points for performance in long jump}
    \item{\code{Psp}}{Points for performance in putting the shot}
    \item{\code{Phj}}{Points for performance in high jump}
    \item{\code{P400m}}{Points for performance in 400 metres}
    \item{\code{P110h}}{Points for performance in 110 metres hurdles}
    \item{\code{Ppv}}{Points for performance in pole vault}
    \item{\code{Pdt}}{Points for performance in discus}
    \item{\code{Pjt}}{Points for performance in javelin}
    \item{\code{P1500}}{Points for performance in 1500 metres}
  }
}
\source{
\url{http://www.decathlon2000.com}
}
\examples{
data(Decathlon, package="GDAdata")
summary(Decathlon[, grep("P.*", names(Decathlon))])
library(ggplot2)
ggplot(Decathlon, aes(Plj)) + geom_histogram()
ggplot(Decathlon, aes(P100m, Plj)) + geom_point()
}
\keyword{datasets}
