% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_forest_plot_annotated.R
\name{create_forest_plot_annotated}
\alias{create_forest_plot_annotated}
\title{Create Forest Plot from Binomial Model}
\usage{
create_forest_plot_annotated(
  data,
  levels,
  labels,
  plot_title,
  plot_subtitle,
  plot_layout,
  colour_rev = F
)
}
\arguments{
\item{data}{output from bionimial model e.g. model <- glm(y ~ x, data = data, family = "binomial")}

\item{levels}{a vector of the unique values (as character strings) that you would like to plot in the order you would like to plot them}

\item{labels}{a vector of character strings which will take the labels of the characters in levels}

\item{plot_title}{a string for the title of the plot}

\item{plot_subtitle}{a string for the subtitle of the plot}

\item{plot_layout}{a patchwork area layout in the form area(t = w, l = x, b = y, r = z), with as many area elements as there are plots.}
}
\value{
a ggplot object
}
\description{
This function will turn your model outputs into a dataframe that is ready to be turned into a forest plot with annotated CI's, p values and variable labels
The order in which your variables will appear in the forest plot is determined by the levels argument and how those variables are labelled is determined by the labels argument.
Remember to include your reference categories in the levels and labels arguments (they will not appear in the model output but are added as part of the function).
}
