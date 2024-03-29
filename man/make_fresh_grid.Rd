% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{make_fresh_grid}
\alias{make_fresh_grid}
\title{Creates a regular grid of rectangular polygons.}
\usage{
make_fresh_grid(bounding_box, dimensions)
}
\arguments{
\item{bounding_box}{The bounding box for this grid}

\item{dimensions}{X and Y dimensions to use. These are counts of cells.}
}
\value{
A spatial polygons object in sf format.
}
\description{
Within a bounding box.
Uses lat-long, unprojected, by default.
}
