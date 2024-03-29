% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{subgraphic}
\alias{subgraphic}
\title{subgraphic: Subplot Function for Multiple Plots}
\usage{
subgraphic(
  plots,
  xmar = NULL,
  ymar = NULL,
  xfig = NULL,
  yfig = NULL,
  overlap = T
)
}
\arguments{
\item{plots}{The plots to be displayed as subplots.}

\item{xmar}{The margins for the x-axis in the subplots. If not specified, the default margins will be used.}

\item{ymar}{The margins for the y-axis in the subplots. If not specified, the default margins will be used.}

\item{xfig}{The figure dimensions for the x-axis in the subplots. If not specified, the default dimensions will be used.}

\item{yfig}{The figure dimensions for the y-axis in the subplots. If not specified, the default dimensions will be used.}

\item{overlap}{A logical value indicating whether the subplots should overlap or be displayed side by side.
If set to TRUE, the subplots will overlap. If set to FALSE, the subplots will be displayed side by side.}
}
\description{
This function allows you to create subplots by arranging multiple plots together in a single figure.
}
\examples{
# Create two plots and display them as subplots
plot1 <- plot(rnorm(100))
plot2 <- plot(runif(100))
subgraphic(plots = list(plot1, plot2))

}
\keyword{multiple}
\keyword{plotting}
\keyword{subfigure}
\keyword{subplot}
