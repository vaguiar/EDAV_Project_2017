\name{EastIndiesTrade}
\alias{EastIndiesTrade}
\docType{data}
\title{
Figures for the trade between England and the East Indies in the 18th century.
}
\description{
The data have been estimated from the graphic in the first edition of Playfair's \emph{Commercial and Political Atlas} by the website 'Me, myself, and BI'.
}
\usage{data(EastIndiesTrade)}
\format{
  A data frame with 81 observations on the following 3 variables.
  \describe{
    \item{\code{Year}}{the data go from 1700 to 1780}
    \item{\code{Exports}}{Exports from England to the East Indies (millions of pounds)}
    \item{\code{Imports}}{Imports to England from the East Indies (millions of pounds)}
      }
}
\source{
\url{http://blog.bissantz.com/vis-a-vis}
}
\examples{
data(EastIndiesTrade, package="GDAdata")
library(ggplot2)
ggplot(EastIndiesTrade, aes(x=Year, y=Exports-Imports)) + geom_line()
}
\keyword{datasets}
