% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{linesInter}
\alias{linesInter}
\title{Interpolate values on a line segment.}
\usage{
linesInter(x, y, z)
}
\arguments{
\item{x}{The x-coordinates of the line segment.}

\item{y}{The y-coordinates of the line segment.}

\item{z}{The target points for interpolation.}
}
\value{
A numeric vector of interpolated values.
}
\description{
This function interpolates values on a line segment defined by x and y coordinates. Given a set of x-coordinates, y-coordinates, and target values, the function calculates the interpolated values at the target points on the line segment.
}
\examples{
x <- c(0, 1, 2, 3)
y <- c(0, 2, 4, 6)
z <- c(0.5, 1.5, 2.5)
interpolated <- linesInter(x, y, z)
interpolated
# Output: 1 3 5
}
