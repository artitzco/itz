% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{reparam}
\alias{reparam}
\title{Reparametrization}
\usage{
reparam(x0, x, y)
}
\arguments{
\item{x0}{The new coordinate value for which the reparametrized value will be calculated.}

\item{x}{A numeric vector of length 2 specifying the original coordinate values.}

\item{y}{A numeric vector of length 2 specifying the corresponding values for the original coordinate values.}
}
\value{
The reparametrized value for \code{x0}.
}
\description{
The \code{reparam} function is used to perform reparametrization of a value based on given coordinates and corresponding values. It calculates a new value \code{x0} based on the linear interpolation between the given coordinates \code{x} and values \code{y}.
}
\examples{
x <- c(0, 1)
y <- c(2, 4)
reparam(0.5, x, y) # Performs reparametrization at x0 = 0.5

}
