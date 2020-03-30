#' Compute pareto fraction of a vector.
#'
#' This isn't the statistical estimator for a Pareto density
#' function's parameters. It calculates a Pareto principle,
#' the 80-20 rule sort of thing. The Pareto density function
#' assumes a minimum value, and this applies to any distribution.
#'
#' @param density This is a sortable, summable vector.
#' @return The fraction, usually presented as $100(1 - fraction, fraction)$.
#' @export
pareto_fraction <- function(density) {
  density <- sort(density)
  cumulant <- cumsum(density) / sum(density)
  count <- length(density)
  reverse <- seq(count, 1) / count
  reverse[min(which(reverse < cumulant))]
}


#' Estimate bandwidth for a density function kernel with Bowman and Azzalini.
#'
#' This is \eqn{\sigma_x\left(\frac{2}{3n}\right)^{1/6}}.
#'
#' @param val A list of coordinates in one dimension.
#' @return A single numeric for the bandwidth.
#'
#' @examples
#' \dontrun{
#' density <- tmap::smooth_map(raster, bandwidth = power_bandwidth(raster))
#' }
#' @export
power_bandwidth <- function(val) {
  one_km <- 1000  # meters
  sd(val) * (2 / (3 * length(val)))^(1/6) / one_km
}


#' Create a map with density estimation
#'
#' Calculating urban fraction by pixel is strange when
#' you have 100 m pixels because urban fraciton is 1000
#' people in a square kilometer, which is 10 people in
#' a square 100 m, so it's more than one large household.
#'
#' Therefore, we use a kernel density estimator for these
#' grid resolutions.
#' @param population_array a Raster of population
#' @param bioko_sf Shapefile for bioko borders
#' @param bandwidth A bandwidth to use for the kernel density estimator
#'   You have to choose this number. Take a look at `power_bandwidth`.
#' @param cutoff The minimum that determines
#' @return A fraction that are above that level
#' @export
density_estimated <- function(
  population,
  bioko_sf,
  bandwidth,
  urban_cutoff = 10
) {
  utm_projection <- "+proj=utm +zone=32N +ellps=WGS84  +no_defs +units=m +datum=WGS84"
  projected <- raster::projectRaster(population, crs = utm_projection)
  bioko_projected <- sf::st_transform(bioko_sf, utm_projection)
  tmaptools::smooth_map(
    projected,
    cover = bioko_projected,
    bandwidth = bandwidth,
    breaks = urban_cutoff
  )
}



#' Calculate urban fraction using a density estimator
#'
#' Calculating urban fraction by pixel is strange when
#' you have 100 m pixels because urban fraciton is 1000
#' people in a square kilometer, which is 10 people in
#' a square 100 m, so it's more than one large household.
#'
#' Therefore, we use a kernel density estimator for these
#' grid resolutions.
#' @param population_array a Raster of population
#' @param bioko_sf Shapefile for bioko borders
#' @param bandwidth A bandwidth to use for the kernel density estimator
#'   You have to choose this number. Take a look at `power_bandwidth`.
#' @param cutoff The minimum that determines
#' @return A fraction that are above that level
#' @export
urban_fraction_by_density_estimator <- function(
  population,
  bioko_sf,
  bandwidth,
  urban_cutoff = 10
  ) {
  density <- density_estimated(population, bioko_sf, bandwidth, urban_cutoff)
  # The kernel density estimator returns both a raster and polygons.
  # We set breaks so that all area greater than the cutoff is in the
  # second polygon.
  fraction_area <- sf::st_area(density$polygons[2, ]) / sf::st_area(bioko_sf)
  units(fraction_area) <- NULL
  fraction_area
}


#' Compute basic summary statistics.
#'
#' Computes total population, maximum population density,
#' percent empty, percent urban, and pareto number.
#' Total population makes sense here because the area is
#' surrounded by water, so that every filled pixel is part
#' of this administrative unit.
#'
#' @param population raster::raster of population data.
#' @param density raster::raster that is density estimated population data
#' @return A list with the statistics, which are maximum value, total
#'     population, pareto fraction, urban fraction, and NA fraction.
#'     Columns are `maximum`, `total`, `empty_fraction`, `pareto_fraction`,
#'     `urban_fraction`, `na_fraction`.
#' @export
summary_statistics <- function(population, density, urban_per_kilometer_sq) {
  population_array <- raster::as.array(population)
  pixel_size_km <- square_meters_per_pixel.raster(population) / 10^6
  zero_cutoff <- 0.1 * pixel_size_km
  raw_urban <- urban_fraction(population, urban_per_kilometer_sq)
  fit_urban <- urban_fraction(density, urban_per_kilometer_sq)
  list(
    total = sum(population_array, na.rm = TRUE),
    side_meters = sqrt(pixel_size_km * 10^6),
    maximum = max(population_array, na.rm = TRUE),
    empty_percent = 100 * sum(population_array < zero_cutoff, na.rm = TRUE) / sum(!is.na(population_array)),
    # Use the fit values for pareto fraction so they are more stable.
    pareto_fraction = pareto_fraction(raster::as.array(density)),
    urban_num = raw_urban[1],
    urban_den = raw_urban[2],
    urban_raw = as.numeric(raw_urban[1]) / raw_urban[2],
    urban_fit_num = fit_urban[1],
    urban_fit_den = fit_urban[2],
    urban_fit = as.numeric(fit_urban[1]) / fit_urban[2],
    na_percent = 100 * sum(is.na(population_array)) / length(population_array)
  )
}
