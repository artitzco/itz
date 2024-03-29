% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{TRY}
\alias{TRY}
\title{Try-Catch Function}
\usage{
TRY(x, y = NULL)
}
\arguments{
\item{x}{The expression to be evaluated.}

\item{y}{The alternative value to return if an error occurs. Defaults to \code{NULL}.}
}
\value{
The result of evaluating the expression \code{x}, or the alternative value \code{y} if an error occurs.
}
\description{
The \code{TRY} function is a convenient wrapper around the \code{tryCatch} function in R. It allows you to evaluate an expression and catch any errors that occur, providing an alternative value \code{y} if an error is encountered.
}
\examples{
TRY(sqrt(4)) # Returns 2
TRY(sqrt(-4)) # Returns NULL
TRY(sqrt(-4), "Error occurred") # Returns "Error occurred"

}
