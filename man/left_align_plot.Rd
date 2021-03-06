% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left_align_plot.R
\name{left_align_plot}
\alias{left_align_plot}
\title{Arrange alignment and save MAE ggplot chart}
\usage{
left_align_plot(
  x,
  source_name,
  width_pixels = 640,
  height_pixels = 450,
  logo_image_path = file.path(system.file("extdata", package = "maecharts"),
    "dh-logo.png")
)
}
\arguments{
\item{x}{The name of the plot you want to export to rsconnect}

\item{source_name}{The text you want to place in the bottom left hand side of your side of your chart add "Source: to the start of your string to specify a source"}

\item{width_pixels}{Width in pixels that you want to save your chart to - defaults to 640}

\item{height_pixels}{Height in pixels that you want to save your chart to - defaults to 450}

\item{logo_image_path}{File path for the logo image you want to use in the right hand side of your chart,
which needs to be a PNG file - defaults to DH logo image that sits within the data folder of your package}
}
\value{
(Invisibly) an updated ggplot object.
}
\description{
Running this function will save your plot with the correct guidelines for publication.
It will left align your title, subtitle and source, add the MAE blocks at the bottom right for use in the r console or rmarkdowns with download buttons.
}
\keyword{left_align_plot}
