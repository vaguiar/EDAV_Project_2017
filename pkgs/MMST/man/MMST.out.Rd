\name{MMST.out}
\alias{MMST.out}
\title{
MMST DATA SET OUTPUT
}
\description{
  Function to output data sets for \emph{Modern Multivariate Statistical Techniques}, by
  A. Izenman (2008), to a single destination
}
\usage{
  MMST.out(dest.folder, datasets = 'all')
}
\arguments{
  \item{dest.folder}{
  String containing path to destination folder for files
}
  \item{datasets}{
  Vector of strings, each component being the name of a desired
  dataset (default is to output all the data sets contained in the package)
  }
}
\details{
  The datasets will be tab delimited with file extension
  \code{.txt}.  This task could be done manually using \code{write.table}, and
  this is what the user should do if they are particular about the
  format of the exported dataset.  The
  reason this function exists is for one to be able to easily export
  every dataset in the book at a single stroke.
}
\value{
NULL
}
\references{
  A. Izenman (2008), \emph{Modern Multivariate Statistical Techniques}, Springer
}
\author{
  Keith Halbert <keith.a.halbert@temple.edu>
}
\note{
  The datasets of class \code{dist} are exported as symmetric matrices
}
\seealso{
  \code{\link{write.table}}
}
\examples{
\dontrun{
  MMST.out('C:/output')                          ## exports all the book's datasets
  MMST.out('C:/output', 'bodyfat')               ## exports single dataset
  MMST.out('C:/output', c('bodyfat', 'tobacco')) ## exports two datasets
  }
}

\keyword{ IO }
