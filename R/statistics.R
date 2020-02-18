#' Compute pareto fraction of a vector.
#'
#' This isn't the statistical estimator for a Pareto density
#' function's parameters. It calculates a Pareto principle,
#' the 80-20 rule sort of thing. The Pareto density function
#' assumes a minimum value, and this applies to any distribution.
#'
#' @param density This is a sortable, summable vector.
#' @result The fraction, usually presented as $100(1 - fraction, fraction)$.
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
#' This is $\sigma_x\left(\frac{2}{3n}\frac)^{1/6}$.
#'
#' @param val A list of coordinates in one dimension.
#' @return A single numeric for the bandwidth.
#'
#' @example
#' density <- tmap::smooth_map(raster, bandwidth = power_bandwidth(raster))
power_bandwidth <- function(val) {
  coords <- tmaptools::coordinates(val)
  one_km <- 1000  # meters
  c(sd(coords[, 1]), sd(coords[, 2])) * (2 / (3 * length(val)))^(1/6) / one_km
}


#' Calculate urban fraction of a raster::raster.
#'
#' The urban fraction is the fraction of land area
#' that has over 1000 people per square kilometer.
#' @param population raster::raster of population data
#' @return Fraction, out of 1, of the land area that
#'     is urban.
#' @export
urban_fraction <- function(population_array, urban_cutoff = 1000) {
  total <- sum(!is.na(population_array))
  urban <- sum(population_array > urban_cutoff, na.rm = TRUE)
  urban / total
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
#' @return A list with the statistics, which are maximum value, total
#'     population, pareto fraction, urban fraction, and NA fraction.
#'     Columns are `maximum`, `total`, `empty_fraction`, `pareto_fraction`,
#'     `urban_fraction`, `na_fraction`.
#' @export
summary_statistics <- function(population) {
  population_array <- as.array(population)
  list(
    maximum = max(population_array),
    total = sum(population_array),
    empty_fraction = length(population_array == 0) / length(population_array),
    pareto_fraction = pareto_fraction(as.array(population_array)),
    urban_fraction = urban_fraction(population_array),
    na_fraction = sum(is.na(population_array)) / length(population_array)
  )
}
