% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnn_plot_functions.R
\name{save_single_explanation_plots}
\alias{save_single_explanation_plots}
\title{Plots raster image(s) with explanations.}
\usage{
save_single_explanation_plots(
  explanation_plots,
  output_path,
  model_name,
  img_names,
  single_img_height,
  single_img_width,
  plot
)
}
\arguments{
\item{explanation_plots}{Explanation plots.}

\item{output_path}{Where to save explanation plots.}

\item{model_name}{Model name.}

\item{img_names}{Original images basenames.}

\item{single_img_height}{Single image height.}

\item{single_img_width}{Single image width.}

\item{plot}{Should explanation be plotted.}
}
\description{
Generates raster image(s) with explanations.
}
