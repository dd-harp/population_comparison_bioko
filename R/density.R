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
population_density_estimator <- function(projected_raster) {
  # Make a point pattern
  raster_sum <- as.integer(raster::cellStats(projected_raster, stat = "sum", na.rm = TRUE))
  pop_raster_im <- maptools::as.im.RasterLayer(projected_raster)
  point_pattern <- spatstat::rpoint(raster_sum, pop_raster_im)
  cat(paste("point pattern", point_pattern$n, "raster sum", raster_sum, "\n"))
  stopifnot(point_pattern$n == raster_sum)
  density <- spatstat::density.ppp(point_pattern, sigma = spatstat::bw.diggle)
  density_scaled <- density * raster_sum / sum(density)

  xrange <- density$xrange[2] - density$xrange[1]
  pixel_width_m <- xrange / ncol(density)
  yrange <- density$yrange[2] - density$yrange[1]
  pixel_height_m <- yrange / nrow(density)
  m2_per_pixel <- pixel_width_m * pixel_height_m

  list(density = density_scaled, m2_per_pixel = m2_per_pixel)
}


#' Urban fraction calculated by converting people to a point pattern.
#'
#' @param pop_raster A raster on lat-long.
#' @param projection A proj.4 string for projection.
#' @param urban_per_kilometer_sq A number of people to define urban (1000, 1500).
#' @return A single density fraction.
#' @export
urban_fraction_by_point_density <- function(pop_raster, projection, urban_per_kilometer_sq) {
  proj_raster <- raster::projectRaster(pop_raster, crs = utm_projection)
  proj_raster <- raster::clamp(proj_raster, lower = 0)
  pdr <- population_density_estimator(proj_raster)
  urban_per_kilometer_sq <- 1500
  urban_per_meter_sq <- urban_per_kilometer_sq / 10^6
  urban_per_pixel_sq <- urban_per_meter_sq * pdr$m2_per_pixel
  sum(pdr$density > urban_per_pixel_sq) / sum(!is.na(pdr$density$v))
}
