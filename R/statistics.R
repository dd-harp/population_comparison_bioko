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
  density_array <- raster::as.array(density)
  pixel_size_km <- square_meters_per_pixel.raster(population) / 10^6
  zero_cutoff <- 0.1 * pixel_size_km
  raw_urban <- urban_fraction_population(population, urban_per_kilometer_sq)
  fit_urban <- urban_fraction_density(density, urban_per_kilometer_sq)
  list(
    total = sum(population_array, na.rm = TRUE),
    side_meters = sqrt(pixel_size_km * 10^6),
    maximum = max(population_array, na.rm = TRUE),
    max_density = max(density_array, na.rm = TRUE),
    empty_percent = 100 * sum(population_array < zero_cutoff, na.rm = TRUE) / sum(!is.na(population_array)),
    # Use the fit values for pareto fraction so they are more stable.
    pareto_fraction = pareto_fraction(density_array),
    urban_num = raw_urban[1],
    urban_den = raw_urban[2],
    urban_raw = as.numeric(raw_urban[1]) / raw_urban[2],
    urban_fit_num = fit_urban[1],
    urban_fit_den = fit_urban[2],
    urban_fit = as.numeric(fit_urban[1]) / fit_urban[2],
    na_percent = 100 * sum(is.na(population_array)) / length(population_array)
  )
}



#' Calculate accuracy, sensitivity, and the like.
#'
#' This takes a truth table and returns accuracy,
#' sensitivity, specificity, positive predictive
#' validity, and negative predictive validity.
#' The truth table is a count of true positive, false positive,
#' true negative, and false negative.
#'
#' @param logic_table A list with names TP, FP, TN, FN.
#' @return A list with `(acc, sens, spec, ppv, npv)`
#' @seealso \link{truth_table}
#' @export
accuracy_profile <- function(logic_table) {
  with(logic_table, {
    list(
      acc = ifelse(TP + TN == 0, 0, (TP + TN) / (TP + TN + FN + FP)),
      sens = ifelse(TP + FN == 0, 0, TP / (TP + FN)),
      spec = ifelse(TN + FP == 0, 0, TN / (TN + FP)),
      ppv = ifelse(TP + FP == 0, 0, TP / (TP + FP)),
      npv = ifelse(TN + FN == 0, 0, TN / (TN + FN))
    )
  })
}




#' Constructs a count of how many entries are in which truth table quadrant.
#'
#' The behavior for NA is important here. If the expected value is NA,
#' then this truth table ignores that entry, even if it's in the observed.
#' We do this because we are looking at map data where NA represents places
#' outside the map. Meanwhile, if the observed is NA, then we count that
#' as false because it won't meet the condition.
#'
#' @param condition A function which, when applied to inputs, gives true and false.
#' @param expected The gold standard for what's true and false.
#' @param observed What is observed as true and false.
#' @seealso \link{accuracy_profile}
#' @export
truth_table <- function(condition, expected, observed) {
  left <- condition(expected)
  right <- condition(observed)
  right <- right[!is.na(left)]
  left <- left[!is.na(left)]
  right[is.na(right)] <- FALSE
  list(
    TP = sum(left & right),
    FP = sum(!left & right),
    FN = sum(left & !right),
    TN = sum(!left & !right)
  )
}
