#' Given a 1km grid for Bioko, figure out how you would
#' remake it.
#'
#' @param grid_1km The raster for a grid in projection.
#' @param unproject Whether to reassign it to a new projection.
#' @return a list of suggested parameters for a grid,
#'     including `width`, `height`, `x_count`, `y_count`,
#'     `bbox`, `interval_x`, and `interval_y`.
#' @export
parameters_of_km_grid <- function(grid_1km, unproject = FALSE) {
  if (unproject) {
    grid_1km <- sf::st_transform(grid_1km, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  }  # else leave it in projection
  coarse_bbox <- sf::st_bbox(grid_1km)
  width <- unname(coarse_bbox["xmax"] - coarse_bbox["xmin"])
  height <- unname(coarse_bbox["ymax"] - coarse_bbox["ymin"])
  number_of_rectangles <- length(grid_1km$geometry)
  aspect <- height / width
  approximate_count <- sqrt(number_of_rectangles / aspect)
  x_count <- round(approximate_count)
  y_count <- round(aspect * approximate_count)
  list(width = width, height = height, x_count = x_count, y_count = y_count,
       bbox = coarse_bbox, interval_x = width / x_count, interval_y = height / y_count)
}


#' Creates a regular grid of rectangular polygons within bounding box
#' Uses lat-long, unprojected, by default.
#'
#' @param bounding_box The bounding box for this grid
#' @param dimensions X and Y dimensions to use. These are counts of cells.
#' @return A spatial polygons object in sf format.
#' @export
make_fresh_grid <- function(bounding_box, dimensions) {
  cell_size <- c(bounding_box["xmax"] - bounding_box["xmin"],
                bounding_box["ymax"] - bounding_box["ymin"]) / dimensions
  offset <- c(bounding_box["xmin"], bounding_box["ymin"]) + 0.5 * cell_size
  gridded <- sp::GridTopology(cellcentre.offset = offset, cellsize = cell_size, cells.dim = dimensions)
  int.layer <- sp::SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(gridded),
    data = data.frame(c(1:prod(dimensions))), match.ID = FALSE)
  names(int.layer) <- "ID"
  sp::proj4string(int.layer) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  int_layer_sf <- sf::st_as_sf(int.layer)
  int_layer_bbox <- sf::st_bbox(int.layer)
  if (max(abs(int_layer_bbox - bounding_box)) > 0.25 * min(cell_size)) {
    cat(paste("Bounding boxes don't match", int_layer_bbox, bounding_box, sep = " "))
  }
  if (length(int_layer_sf$geometry) != prod(dimensions)) {
    stop("Geometry generated has wrong number of grid cells")
  }
  int_layer_sf
}


#' Turns each population entry into a lat-long coordinate.
#' @param grid_100m A 100m grid.
#' @param local_directory Where external data is stored.
#' @return An sf points file with population as features.
#' @export
register_bimep_population <- function(grid_100m, local_directory = "inst/extdata") {
  fine_df <- data.frame(grid_100m)
  lookup <- 1:nrow(fine_df)
  names(lookup) <- fine_df$mapaID
  popm_csv <- fs::path(local_directory, "source", "popm.csv")
  pops <- read.table(popm_csv, stringsAsFactors = FALSE, header = TRUE, sep = ",")
  projected_cell_center <- lapply(1:nrow(pops), function(idx) {sf::st_point(c(pops[idx, "xcoord"], pops[idx, "ycoord"]))})
  features <- sf::st_sf(st_as_sfc(projected_cell_center), crs = "+proj=utm +zone=32N +ellps=WGS84  +no_defs +units=m +datum=WGS84")
  features_latlong <- sf::st_transform(features, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  features_latlong
}


#' Given an HRSL raster, convert it into points in space.
#'
#' @param hsrl_raster This is a raster read from the HRSL geotiff.
#' @return An sf points dataset with populations on the points.
#' @export
hrsl_points <- function(hrsl_raster) {
  # Make 3 columns, (x, y, population)
  hrsl_xym <- raster::rasterToPoints(hrsl)
  # Make Points with one feature, the population.
  hrsl_sf <- sf::st_as_sf(x = data.frame(hrsl_xym), coords = 1:2)
  sf::st_crs(hrsl_sf) <- sp::CRS("+init=epsg:4326")
  hrsl_sf
}


#' Given spatial points of population, assign them to shapes.
#' This transforms the grid into the same space as the points
#' dataset and then assigns the sum of points to the grid.
#'
#' @param grid A regular grid.
#' @param points An sf points dataset.
#' @return An sf grid with points assigned or
#'     NA where there are no points.
#' @export
assign_to_grid <- function(grid, points) {
  sf::st_transform(grid, sp::st_crs(points))
  # This will have NA where there are no points
  points_on_grid <- aggregate(points, grid, sum)
  points_on_grid
}
