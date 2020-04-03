#' Script to compute summary statistics with uncertainty from alignment of grid.
#'
#' Given gold standard data, which has lat-long GPS coordinates for each household,
#' align it to a grid and compute summary statistics for it, such as percent empty, percent urban,
#' and maximum. Compute an uncertainty in these values by repeatedly shifting
#' the grid so that we know how much slight translations of the grid can
#' affect results.
#'
#' @param grid This says which grid to use, among hrsl, landscan, or worldpop.
#' @param shifts How many times to shift the grid in x and y. The number of loops internally
#'     will be shifts * shifts.
#' @param filename A file to which to save results.
#' @export
summary_statistics_multiple_grids <- function(grid, shift_cnt, output) {
  available_grids <- c(
    hrsl = read_hrsl,
    landscan = read_landscan,
    worldpop = read_worldpop
  )
  stopifnot(grid %in% names(available_grids))
  shift_cnt <- as.integer(shift_cnt)
  stopifnot(shift_cnt > 0)
  output <- fs::path_real(output)
  # Prove at the start that we can write this filename so we don't find out
  # after doing all of the computation.
  write(1:3, output)
  file.remove(output)

  local_directory <- "ext/instdata"
  urban_cutoff <- 1500

  geolocated <- read_bimep_point_data(local_directory)
  admin_sf <- sf::st_read(fs::path(local_directory, "source", "bioko.shp"))
  base_grid <- available_grids[tolower(grid)](local_directory)
  grid <- clean_grid(base_grid, admin_sf, local_directory)

  summarizer <- function(pixel_fraction) {
    summarize(geolocated, admin_sf, grid, pixel_fraction, urban_cutoff, local_directory)
  }
  summaries <- summarize_over_shifts(shift_cnt, summarizer)
  summaries["grid"] <- grid
  summaries["shift_cnt"] <- shift_cnt
  write.csv(summaries, output, stringsAsFactors = FALSE, quote = FALSE, row.names = FALSE)
}


#' Given an input grid, crop it to the admin unit and give us a few pixels of border.
#'
#' @param grid A raster to which to align.
#' @param admin_sf An sf shapefile that is the admin for cropping.
#' @return A raster that is ready to use as a template.
clean_grid <- function(grid, admin_sf) {
  hrsl_raster_crop <- raster::crop(grid, admin_sf, snap = "out")
  hrsl_zero_mask <- raster::rasterize(admin_sf, hrsl_raster_crop, field = 0)
  hrsl_raster_zero <- raster::cover(hrsl_raster_crop, hrsl_zero_mask)
  # Add a pixel because we will shift this around.
  raster::extend(hrsl_raster_zero, c(1, 1))
}


#' Compute summary statistics for geolocated data aligned to a grid.
#'
#' @param geolocated the GPS gold standard data.
#' @param admin_sf The boundary of the administrative unit as an sf shapefile.
#' @param grid A raster::raster to which to align the geolocated data.
#' @param shift A length-2 vector with the fraction of a pixel to shift the grid in x and y.
#' @param urban_cutoff A density above which summary statistics will call a place urban.
#' @return A list of summary statistics.
#' @export
summarize <- function(geolocated, admin_sf, grid, shift, urban_cutoff) {
  shifted_grid <- raster::shift(
    grid, dx = shift[1] * raster::xres(grid), dy = shift[2] * raster::yres(grid))
  gridded <- geolocated_on_grid(shifted_grid, geolocated, admin_sf)
  utm_projection <- "+proj=utm +zone=32N +ellps=WGS84  +no_defs +units=m +datum=WGS84"
  proj_raster <- raster::projectRaster(gridded, crs = utm_projection, method = "ngb")
  pop_density_raster <- density_from_disc(proj_raster)
  summary_statistics(proj_raster, pop_density_raster, urban_cutoff)
}


#' Loops over the given grid shifts and runs the summarizer on each shift.
#'
#' The problem is that the inner loop can be slow, so we separate the
#' loop logic for testing that it can shift properly and accumulate results.
#'
#' @param shift_cnt The number of shifts in x and y, so the loop length is shift_cnt * shift_cnt.
#' @param summarizer A function that takes the current shift fraction and return a summary.
#' @return a data.frame with the results
#' @export
summarize_over_shifts <- function(shift_cnt, summarizer) {
  shifts <- t(matrix(c(rep(1:shift_cnt, shift_cnt), rep(1:shift_cnt, each = shift_cnt)), ncol = 2))
  summaries <- vector(mode = "list", length = ncol(shifts))
  for (shift_idx in 1:ncol(shifts)) {
    pixel_fraction <- (shifts[, shift_idx] - 1) / shift_cnt
    summaries[shift_idx] <- summarizer(pixel_fraction)
  }
  as.data.frame(do.call(rbind, summaries))
}
