% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{newCount}
\alias{newCount}
\title{Create a counter object}
\usage{
newCount(start = NA, step = 1, limits = NA)
}
\arguments{
\item{start}{The starting value of the counter. Default is NA.}

\item{step}{The increment or decrement step value. Default is 1.}

\item{limits}{A numeric vector indicating the lower and upper limits of
the counter. If specified, the counter will wrap around within the limits.
Default is NA, indicating no limits.}
}
\value{
A new counter object.
}
\description{
This function creates a new counter object that can be used to track and
manipulate a numeric count. The counter can be incremented or decremented
using the \code{plus} and \code{less} functions respectively. The current count can
be retrieved using the \code{get} function.
}
\examples{
# Create a counter with default values
count <- newCount()
count$get() # Returns the current count: 0
count$plus() # Increment the count
count$get() # Returns the updated count: 1
count$less() # Decrement the count
count$get() # Returns the updated count: 0

# Create a counter with custom values
count <- newCount(start = 5, step = 2, limits = c(0, 9))
count$get() # Returns the current count: 5
count$plus() # Increment the count with wrap-around
count$get() # Returns the updated count: 7
count$less() # Decrement the count with wrap-around
count$get() # Returns the updated count: 5

}
