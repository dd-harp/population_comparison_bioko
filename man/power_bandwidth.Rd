% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{power_bandwidth}
\alias{power_bandwidth}
\title{Estimate bandwidth for a density function kernel with Bowman and Azzalini.}
\usage{
power_bandwidth(val)
}
\arguments{
\item{val}{A list of coordinates in one dimension.}
}
\value{
A single numeric for the bandwidth.
}
\description{
This is \eqn{\sigma_x\left(\frac{2}{3n}\right)^{1/6}}.
}
\examples{
\dontrun{
density <- tmap::smooth_map(raster, bandwidth = power_bandwidth(raster))
}
}
