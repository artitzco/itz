% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{Bar}
\alias{Bar}
\title{Bar Function}
\usage{
Bar(name, ...)
}
\arguments{
\item{name}{(character) The name of the parameter.}

\item{...}{Additional arguments to be passed.}
}
\value{
(NULL)
}
\description{
Description:
This function creates a new global parameter for controlling bar plot properties.
}
\examples{
Bar("mybar", alp.bar = 1, col.bar = "#A6A6A6", lwd.bar = 2, lty.bar = 1, horizontal = TRUE)

}
\references{
For more information, refer to the documentation of the \code{newGlobalParam} function.
}
\seealso{
\code{\link{newGlobalParam}}
}
\keyword{bar}
\keyword{global}
\keyword{parameter}
