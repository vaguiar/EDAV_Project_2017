\name{lorenz.curve}
\alias{lorenz.curve}
\title{Lorenz Curve}
\description{
This function plots the Lorenz curve that is a graphical representation of the cumulative
distribution function. A user can choose for the Lorenz curve with single (default) 
or multiple weighting of data, for example, taking into account for single or multiple 
legislature representatives.   

The input data should be a data frame with 2 columns. The first column will be 
treated as data vector, and the second column to be treated as a weight vector. Alternatively, 
data and weight can be entered as separate one-column vectors.
}

\usage{
lorenz.curve(data, weight=NULL, mul=FALSE, plot.it=TRUE, 
   main=NULL, xlab=NULL, ylab=NULL, xlim=c(0,1), ylim=c(0,1), ... )
}
\arguments{
  \item{data}{input data. If the argument is an array, a matrix, a data.frame, or a list with two or more 
              columns, then the first column will be a treated as a data vector, and the second column to be 
              treated as a weight vector. A separate weight vector is then ignored and not required. 
              If the argument is a single column vector, then a user must enter a separate single-column weight vector. 
              NA or character is not allowed.}

  \item{weight}{single column vector contains factors of single or multiple weights. Ignored if already included in
                the data argument. NA or character is not allowed.}

  \item{mul}{logical value indicates whether the Lorenz Curve with multiple weight is to be plotted. Default is single.}
  \item{plot.it}{logical value indicates whether the Lorenz Curve should be plotted on screen. Default is to plot.}
  \item{main}{Title of Lorenz Curve. Only required if user wants to override the default value.}
  \item{xlab}{label of x-axis. Only required if user wants to override the default value.}
  \item{ylab}{label of y-axis. Only required if user wants to override the default value.}
  \item{xlim}{plotting range of x-axis. Only required if user wants to override the default value.}
  \item{ylim}{plotting range of y-axis. Only required if user wants to override the default value.}
  \item{\dots}{other graphical parameters to be passed to the \code{plot} function.}
}

\value{
  \item{x}{Culmulative fraction of the data argument.}
  \item{y}{Culmulative fraction of the weight argument.}
  \item{gini}{The Gini index of the input data.}
  \item{relative.mean.dev}{Relative Mean Deviation of the input data.}
  \item{L(1/2)}{Median of the culmulative fraction sum of the data.}
}

\references{
Gastwirth, J. L.(1972) \emph{The Estimation of the Lorenz Curve and Gini Index}, 
The Review of Economics and Statistics, Vol. 54, No. 3. (August 1972), pp. 306-316.\cr
}

\author{Man Jin, Wallace W. Hui, Yulia R. Gel, Joseph L. Gastwirth}

\seealso{\code{\link{gini.index}}}

\examples{

## Population Data of ratio of number of senators (second column) and 
## representatives (third column) to population size (first column) in 1963
## First column is treated as data argument.

data(data1963)

## Single weight Lorenz Curve using number of senators as weight argument.
lorenz.curve(data1963)

## Multiple weight Lorenz Curve using number of senators as weight argument.
lorenz.curve(data1963, mul=TRUE)

## Multiple weight Lorenz Curve using number of representatives 
## as weight argument.
lorenz.curve(data1963[,"pop1963"], data1963[,"rep1963"], mul=TRUE)

}
\keyword{dplot}
