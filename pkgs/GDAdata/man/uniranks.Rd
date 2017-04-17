\name{uniranks}
\alias{uniranks}
\docType{data}
\title{
The Guardian University League Table 2013
}
\description{
The Guardian newspaper in the UK publishes a ranking of British universities each year and it reported these data in May, 2012 as a guide for 2013.
}
\usage{data(uniranks)}
\format{
  A data frame with 120 observations on the following 13 variables.
  \describe{
    \item{\code{Rank}}{Rank of the University}
    \item{\code{Institution}}{University name}
    \item{\code{UniGroup}}{Universities can be a member of one of five groups, \code{1994 Group}, \code{Guild HE}, \code{Million+}, \code{Russell}, \code{University Alliance}, or none}
    \item{\code{HesaCode}}{University's Higher Education Statistics Agency code}
    \item{\code{AvTeachScore}}{Average Teaching Score}
    \item{\code{NSSTeaching}}{University's National Student Survey teaching score}
    \item{\code{NSSOverall}}{University's NSS overall score}
    \item{\code{SpendPerStudent}}{University expenditure per student (depends on subject)}
    \item{\code{StudentStaffRatio}}{Student to Staff ratio}
    \item{\code{CareerProspects}}{Proportion of graduates in appropriate level employment or full-time study within six months of graduation}
    \item{\code{ValueAddScore}}{''Based upon a sophisticated indexing methodology that tracks students from enrolment to graduation, qualifications upon entry are compared with the award that a student receives at the end of their studies.'' (Guardian)}
    \item{\code{EntryTariff}}{Value dependent on the average points needed to get on the university's courses}
    \item{\code{NSSFeedback}}{University's NSS feedback score}
  }
}
\source{
\url{http://www.theguardian.com/news/datablog/2012/may/22/university-guide-2013-guardian-data}}
\examples{
data(uniranks, package="GDAdata")
with(uniranks, table(UniGroup))
library(ggplot2)
ggplot(uniranks, aes(x=NSSTeaching, y=NSSFeedback)) + geom_point()
ggplot(uniranks, aes(x=UniGroup, y=SpendPerStudent)) + geom_boxplot()
}
\keyword{datasets}
