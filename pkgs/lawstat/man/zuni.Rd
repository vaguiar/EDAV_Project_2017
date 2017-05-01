\name{zuni}
\alias{zuni}
\docType{data}
\title{The Zuni data from the law case: Zuni Public School v. United States
Department of Education}
\description{
Number of students and available revenue per pupil
in each school district in New Mexico
}
\usage{zuni}
\format{
A multivariate time series with 89 observations on 4 variables: District, Revenue and Mem
(number of students). The object is of class "mts".
}
\details{
The Zuni data come from a law case "The Zuni Public School District
No. 89, Gallup-McKinley County Public School District No. 1,
Petitioners v. United States Department of Education" concerning
whether the revenue per pupil data satisfied a standard for "equal"
expenditures per-pupil in a state. This classification determines
whether most of the federal money given to the state under the law
goes to the state or to the local school districts.
}
\source{
Gastwirth, J. L. (2006). \emph{A Sixty Million Dollar Statistical
Issue Arising in the Interpretation and Calculation of a Measure of
Relative Disparity Mandated by Law: Zuni Public School District 89
v. U.S. Department of Education.} (working paper, Department of Statistics,
George Washington University).
}
\examples{
data(zuni)
hist(zuni[,"Revenue"], br=40, col="blue", main="The Zuni Revenue")
}
\keyword{datasets}
