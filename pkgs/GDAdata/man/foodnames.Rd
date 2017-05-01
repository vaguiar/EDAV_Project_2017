\name{foodnames}
\alias{foodnames}
\docType{data}
\title{
Nutritional value of food.
}
\description{
Nutritional value of different foods based on standard serving sizes.
}
\usage{data(foodnames)}
\format{
  A data frame with 961 observations on the following 9 variables.
  \describe{
    \item{\code{Name}}{name of food (not unique)}
    \item{\code{Measure}}{serving description}
    \item{\code{Fat.grams}}{grams of fat in a standard serving}
    \item{\code{Food.energy.calories}}{calories per serving}
    \item{\code{Carbohydrates.grams}}{grams of carbohydrates per serving}
    \item{\code{Protein.grams}}{grams of protein per serving}
    \item{\code{Cholesterol.mg}}{cholesterol in mg per serving}
    \item{\code{weight.grams}}{weight in grams of a standard serving}
    \item{\code{Saturated.fat.grams}}{grams of saturated fat per serving}
  }
}
\source{
The data are used in A. Izenman (2008), Modern Multivariate Statistical Techniques, Springer\cr
and are available on the accompanying website
\url{http://astro.temple.edu/~alan/MMST/}
}
\examples{
data(foodnames, package="GDAdata")
summary(foodnames)
library(ggplot2)
ggplot(foodnames, aes(Fat.grams, Saturated.fat.grams)) + geom_point()
}
\keyword{datasets}
