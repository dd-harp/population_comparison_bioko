
bounding_box_utm <- function(grid_1km) {
  coarse_bbox <- sf::st_bbox(grid_1km)
  coarse_bbox
}


#' Given a 1km grid for Bioko, figure out how you would
#' remake it.
parameters_of_km_grid <- function(grid_1km, unproject = FALSE) {
  if (unproject) {
    grid_1km <- sf::st_transform(grid_1km, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  }  # else leave it in projection
  coarse_bbox <- bounding_box_utm(grid_1km)
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


register_bimep_population <- function(grid_100m) {
  fine_df <- data.frame(grid_100m)
  lookup <- 1:nrow(fine_df)
  names(lookup) <- fine_df$mapaID
  pops <- read.table("vignettes/popm.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
  projected_cell_center <- lapply(1:nrow(pops), function(idx) {sf::st_point(c(pops[idx, "xcoord"], pops[idx, "ycoord"]))})
  features <- sf::st_sf(st_as_sfc(projected_cell_center), crs = "+proj=utm +zone=32N +ellps=WGS84  +no_defs +units=m +datum=WGS84")
  features_latlong <- sf::st_transform(features, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
}
