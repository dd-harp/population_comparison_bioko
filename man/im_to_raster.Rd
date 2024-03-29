% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{im_to_raster}
\alias{im_to_raster}
\title{Convert a spatstat im to a raster image}
\usage{
im_to_raster(spat_im, projection)
}
\arguments{
\item{spat_im}{a spatstat im object.}

\item{projection}{a proj.4 projection string for a CRS}
}
\value{
A raster::raster in projection
}
\description{
Convert a spatstat im to a raster image
}
