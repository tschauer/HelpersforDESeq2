% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plottingPCA.R
\name{plottingPCA}
\alias{plottingPCA}
\title{plottingPCA}
\usage{
plottingPCA(
  my_data,
  xcomp = 1,
  ycomp = 2,
  conditions,
  pca_colors = c("#999999", "#0072B2", "#CC79A7", "#009E73", "#E69F00", "#D55E00",
    "#56B4E9", "#F0E442"),
  main_title = "PCA",
  quantiles = c(0, 1),
  show_labels = TRUE,
  point_size = 1.1,
  my_xlimits = c(-100, 100),
  my_ylimits = c(-100, 100)
)
}
\description{
PCA analysis and plotting based on read counts
}
