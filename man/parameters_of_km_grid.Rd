% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{parameters_of_km_grid}
\alias{parameters_of_km_grid}
\title{Given a 1km grid for Bioko, figure out how you would
remake it.}
\usage{
parameters_of_km_grid(grid_1km, unproject = FALSE)
}
\arguments{
\item{grid_1km}{The raster for a grid in projection.}

\item{unproject}{Whether to reassign it to lat-long
instead of a projected coordinate system.}
}
\value{
a list of suggested parameters for a grid,
    including `width`, `height`, `dimensions`,
    `bbox`, and `interval`, which is the size of each square.
}
\description{
Given a 1km grid for Bioko, figure out how you would
remake it.
}
