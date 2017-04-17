\name{gini.index}
\alias{gini.index}
\encoding{latin1}
\title{Measures of Relative Variability - Gini Index}
\description{
This function measures relative inequality (or relative variation)
of the data using the Gini Index. NAs from the data are omitted.
}
\usage{
gini.index(x)
}
\arguments{
  \item{x}{the input data.}
}
\value{
  A list with the following numeric components.
  \item{statistic}{The Gini Index of the data.}
  \item{parameter}{the mean difference of a set of numbers.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Gastwirth, J. L.(1988) \emph{Statistical Reasoning in Law and
Public Policy Vol 1}, Boston; Toronto, Academic Press.\cr

Gini, C. \emph{Variabilita e mutabilita} (1912) Reprinted in
\emph{Memorie di metodologica statistica (Ed. Pizetti E,
Salvemini, T}. Rome: Libreria Eredi Virgilio Veschi (1955).
English translation in Metron, 2005,63,(1) 3-38
}
\author{Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{cd}}, \code{\link{j.maad}}, \code{\link{lorenz.curve}}}
\examples{
## The Baker v. Carr Case: one-person-one-vote decision. 
## Measure of Relative Inequality of Population data in 33 districts 
## of the Tennessee Legislature in 1900 and 1972. See 
## popdata (see Gastwirth (1988)).

data(popdata)
gini.index(popdata[,"pop1900"])

## Measures of Relative Variability  - Gini Index
##
## data:  popdata[, "pop1900"]
## Gini Index = 0.1147, delta = 3389.765

gini.index(popdata[,"pop1972"])

## Measures of Relative Variability  - Gini Index
##
## data:  popdata[, "pop1972"]
## Gini Index = 0.0055, delta = 1297.973
}
\keyword{htest}
