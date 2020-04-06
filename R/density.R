#' Estimate density of population using density estimator on points.
#'
#' This takes a grid of population in latitude and longitude
#' and turns it into a point per person in order to estimate
#' the density of people.
#'
#' @param projected_raster A raster in projected coordinates.
#' @return A list with a density raster and `m2_per_pixel`, which
#'   is square meters per pixel.
#' @export
population_count_estimator <- function(projected_raster) {
  # Make a point pattern
  raster_sum <- as.integer(raster::cellStats(projected_raster, stat = "sum", na.rm = TRUE))
  pop_raster_im <- maptools::as.im.RasterLayer(projected_raster)
  point_pattern <- spatstat::rpoint(raster_sum, pop_raster_im)
  stopifnot(point_pattern$n == raster_sum)
  density <- spatstat::density.ppp(
    point_pattern,
    sigma = spatstat::bw.diggle,
    dimyx = c(raster::ncol(projected_raster), raster::nrow(projected_raster))
    )
  density * raster_sum / sum(density)
}


#' Estimate density of population using density estimator on the image.
#'
#' This takes the original grid of populations and turns it into
#' a point pattern with point point per cell, but each point has
#' a mark that is the value of the cell.
#'
#' @param projected_raster A raster in projected coordinates.
#' @return A list with a density raster and `m2_per_pixel`, which
#'   is square meters per pixel.
#' @export
population_density_estimator <- function(projected_raster) {
  pop_raster_im <- maptools::as.im.RasterLayer(projected_raster)
  window <- spatstat::Window(pop_raster_im)
  xy <- spatstat::raster.xy(window, drop = TRUE)
  point_pattern <- spatstat::ppp(x = xy$x, y = xy$y, window = window)
  density <- spatstat::density.ppp(
    point_pattern,
    sigma = spatstat::bw.diggle,
    dimyx = dim(pop_raster_im),
    weights = pop_raster_im
  )
  density
}


blurring <- function(ras_ras) {
  # sigma is half of the radius of a circle with 1 km^2 area.
  sigma <- 0.5 * sqrt(10^6 / pi)
  raster_im <- maptools::as.im.RasterLayer(ras_ras)
  smoothed <- spatstat::blur(
    raster_im,
    sigma = sigma,
    kernel = "disc",
    normalise = FALSE,  # The normalization is cool but gives crazy answers.
    bleed = FALSE  # This sets the NA cutoff for us.
    )
  raster::raster(maptools::as.SpatialGridDataFrame.im(smoothed))
}


#' Find the average density within a 1 square km disc around each pixel
#'
#' Given a raster that is projected to a grid measured in meters,
#' take each pixel and average its values with all pixels within
#' a disc that is 1 square kilometer in area.
#'
#' @param projected_raster a raster::raster that is projected to
#'   have meters as units.
#' @return A raster::raster that is similarly projected but now smoothed.
#' @export
density_from_disc <- function(projected_raster) {

  density_raster <- blurring(projected_raster)

  # Now account for NA values near the edges by blurring a matrix of ones.
  ones_raster <- raster::setValues(raster::raster(projected_raster), 1)
  ones_raster <- raster::mask(ones_raster, projected_raster)
  ones_density <- blurring(ones_raster)

  pixel_area_km <- square_meters_per_pixel.raster(projected_raster) / 10^6
  density_raster / (ones_density * pixel_area_km)
}


#' Calculate square meters per pixel for a spatstat im
#'
#' @param spatstat_im The raster image in spatstat im format.
#' @return A single value for square meters per pixel
#' @export
square_meters_per_pixel.im <- function(spatstat_im) {
  xrange <- spatstat_im$xrange[2] - spatstat_im$xrange[1]
  pixel_width_m <- xrange / ncol(density)
  yrange <- spatstat_im$yrange[2] - spatstat_im$yrange[1]
  pixel_height_m <- yrange / nrow(spatstat_im)
  pixel_width_m * pixel_height_m
}


#' Calculate square meters per pixel for a raster::raster
#'
#' @param spatstat_im The raster image in raster::raster format.
#' @return A single value for square meters per pixel
#' @export
square_meters_per_pixel.raster <- function(pop_raster) {
  if (raster::isLonLat(pop_raster)) {
    km_squared <- 10^6
    area <- mean(raster::getValues(raster::area(pop_raster))) * km_squared
  } else {
    xrange <- raster::xFromCol(pop_raster, 2) - raster::xFromCol(pop_raster, 1)
    yrange <- raster::yFromRow(pop_raster, 2) - raster::yFromRow(pop_raster, 1)
    area <- abs(xrange * yrange)  # columns can be flipped
  }
  area
}


#' Convert a spatstat im to a raster image
#'
#' @param spat_im a spatstat im object.
#' @param projection a proj.4 projection string for a CRS
#' @return A raster::raster in projection
#' @export
im_to_raster <- function(spat_im, projection) {
  values <- spatstat::as.matrix.im(spat_im)
  raster::raster(
    values[nrow(values):1, ],
    xmn = spat_im$xrange[1],
    xmx = spat_im$xrange[2],
    ymn = spat_im$yrange[1],
    ymx = spat_im$yrange[2],
    crs = projection
  )
}


#' Urban fraction from a population raster
#'
#' This assumes that raster values are population in that grid location,
#' not density of the population in the vicinity of that grid location.
#' It divides population by the area of the grid pixel.
#'
#' @param pop_raster A raster::raster in projection
#' @param urban_per_kilometer_sq A number of people to define urban (1000, 1500).
#' @return A vector with c(numerator, denominator) of pixel counts.
#' @export
urban_fraction_population <- function(density_raster, urban_per_kilometer_sq) {
  urban_per_meter_sq <- urban_per_kilometer_sq / 10^6
  urban_per_pixel_sq <- urban_per_meter_sq * square_meters_per_pixel.raster(density_raster)
  vals <- raster::getValues(density_raster)
  c(sum(vals > urban_per_pixel_sq, na.rm = TRUE), sum(!is.na(vals)))
}


#' Urban fraction from a density raster
#'
#' This assumes that the raster values are density per square kilometer.
#' It counts raster values above a cutoff.
#'
#' @param pop_raster A raster::raster in projection
#' @param urban_per_kilometer_sq A number of people to define urban (1000, 1500).
#' @return A vector with c(numerator, denominator) of pixel counts.
#' @export
urban_fraction_density <- function(density_raster, urban_per_kilometer_sq) {
  vals <- raster::getValues(density_raster)
  c(sum(vals > urban_per_kilometer_sq, na.rm = TRUE), sum(!is.na(vals)))
}
