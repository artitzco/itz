% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{auxiliarWD}
\alias{auxiliarWD}
\title{Set or retrieve the working auxiliary directory}
\usage{
auxiliarWD(file = "")
}
\arguments{
\item{file}{The file path for the working auxiliary directory.}
}
\value{
The current working auxiliary directory if no argument is provided. Otherwise, NULL is returned.
}
\description{
The \code{auxiliarWD} function is used to set or retrieve the working auxiliary directory. If no argument is provided, it returns the current working directory. If a file path is provided as an argument, it sets the working auxiliary directory to that path.
}
\examples{
auxiliarWD() # Retrieves the current working auxiliary directory
auxiliarWD("path/to/directory") # Sets the working auxiliary directory to the specified path

}
