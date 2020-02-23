#' Given a 1km grid for Bioko, figure out how you would
#' remake it.
#'
#' @param grid_1km The raster for a grid in projection.
#' @param unproject Whether to reassign it to lat-long
#'     instead of a projected coordinate system.
#' @return a list of suggested parameters for a grid,
#'     including `width`, `height`, `dimensions`,
#'     `bbox`, and `interval`, which is the size of each square.
#' @export
parameters_of_km_grid <- function(grid_1km, unproject = FALSE) {
  if (unproject) {
    grid_1km <- sf::st_transform(
      grid_1km,
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      )
  } # else leave it in projection
  coarse_bbox <- sf::st_bbox(grid_1km)
  width <- unname(coarse_bbox["xmax"] - coarse_bbox["xmin"])
  height <- unname(coarse_bbox["ymax"] - coarse_bbox["ymin"])
  number_of_rectangles <- length(grid_1km$geometry)
  aspect <- height / width
  approximate_count <- sqrt(number_of_rectangles / aspect)
  x_count <- round(approximate_count)
  y_count <- round(aspect * approximate_count)
  list(
    width = width, height = height,
    dimensions = c(x_count, y_count),
    bbox = coarse_bbox,
    interval = c(width / x_count, height / y_count)
  )
}


#' Creates a regular grid of rectangular polygons.
#'
#' Within a bounding box.
#' Uses lat-long, unprojected, by default.
#'
#' @param bounding_box The bounding box for this grid
#' @param dimensions X and Y dimensions to use. These are counts of cells.
#' @return A spatial polygons object in sf format.
#' @export
make_fresh_grid <- function(bounding_box, dimensions) {
  cell_size <- c(
    bounding_box["xmax"] - bounding_box["xmin"],
    bounding_box["ymax"] - bounding_box["ymin"]
  ) / dimensions
  offset <- c(bounding_box["xmin"], bounding_box["ymin"]) + 0.5 * cell_size
  gridded <- sp::GridTopology(
    cellcentre.offset = offset,
    cellsize = cell_size,
    cells.dim = dimensions
    )
  int.layer <- sp::SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(gridded),
    data = data.frame(c(1:prod(dimensions))), match.ID = FALSE
  )
  names(int.layer) <- "ID"
  sp::proj4string(int.layer) <-
    "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  int_layer_sf <- sf::st_as_sf(int.layer)
  int_layer_bbox <- sf::st_bbox(int.layer)
  if (max(abs(int_layer_bbox - bounding_box)) > 0.25 * min(cell_size)) {
    cat(paste(
      "Bounding boxes don't match", int_layer_bbox, bounding_box, sep = " "
      ))
  }
  if (length(int_layer_sf$geometry) != prod(dimensions)) {
    stop("Geometry generated has wrong number of grid cells")
  }
  int_layer_sf
}


#' Associate BIMEP index with its map ID.
#'
#' @param grid_100m The grid with `mapaID` defined.
#' @return A vector of indexes and associated names.
#' @export
bimep_named_vector <- function(grid_100m) {
  fine_df <- data.frame(grid_100m)
  lookup <- 1:nrow(fine_df)
  names(lookup) <- fine_df$mapaID
  lookup
}


#' Turns each population entry into a lat-long coordinate.
#'
#' This reads the `popm.csv` of population at 100m grid points.
#' It produces a vector file with population as a feature.
#'
#' @param local_directory Where external data is stored.
#' @return An sf points file with population as features.
#' @export
bimep_population_as_points <- function(local_directory = "inst/extdata") {
  popm_csv <- fs::path(local_directory, "source", "popm.csv")
  pops <- read.table(
    popm_csv,
    stringsAsFactors = FALSE, header = TRUE, sep = ",")
  projected_cell_center <- lapply(1:nrow(pops), function(idx) {
    sf::st_point(c(pops[idx, "xcoord"], pops[idx, "ycoord"]))
  })
  utm_projected_crs <- "+proj=utm +zone=32N +ellps=WGS84  +no_defs +units=m +datum=WGS84"
  features <- sf::st_sf(
    st_as_sfc(projected_cell_center),
    pop = pops$pop100,
    crs = utm_projected_crs
    )
  features_latlong <- sf::st_transform(
    features,
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )
  features_latlong
}


#' Aligns long-lat per-house data to a given raster grid.
#'
#' @param grid A raster::raster to which to align
#' @param bioko_sf A shapefile that defines where population is zero.
#' @param local_directory Location of the directory where the file is found.
#' @return A raster::raster with the new data on longlat, na in ocean,
#'   zero or a pop value on land.
#' @export
bimep_on_grid <- function(grid, bioko_sf, local_directory = "inst/extdata") {
  popm_csv <- fs::path(local_directory, "source", "hh_pop_bioko.csv")
  pops <- read.table(
    popm_csv,
    stringsAsFactors = FALSE, header = TRUE, sep = ",")
  without_index <- pops[, !names(pops) %in% c("index")]
  names(without_index) <- c("lon", "lat", "pop")
  projected_cell_center <- lapply(1:nrow(pops), function(idx) {
    sf::st_point(c(pops[idx, "lon"], pops[idx, "lat"]))
  })
  features <- sf::st_sf(
    st_as_sfc(projected_cell_center),
    pop = without_index$pop,
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
  house_and_na <- raster::rasterize(features, grid, fun = sum, field = "pop")
  hrsl_zero_mask <- raster::rasterize(bioko_sf, grid, field = 0)
  house_and_zero <- raster::cover(house_and_na, hrsl_zero_mask)
  names(house_and_zero) <- c("BIMEP")
  house_and_zero
}


row_first <- function(x, row_width, column_width) {
  c(x %% row_width, x - row_width * (x %% row_width))
}

column_first <- function(x, row_width, column_width) {
  c(x - column_width * (x %% column_width), x %% column_width)
}

#' Read BIMEP population as a raster.
#'
#' This method shows that we really don't have the same raster as
#' the original BIMEP cells. This grid and the BIMEP grid aren't aligned.
#'
#' We don't have the original raster, unfortunately, so we try
#' to rebuild it here. The data is organized into sections and then
#' areas within the section.
#' @export
bimep_population_as_raster <- function(grid_info, lat_long_proj, point_population) {
  raster100 <- raster::raster(
    xmn = grid_info$bbox["xmin"],
    xmx = grid_info$bbox["xmax"],
    ymn = grid_info$bbox["ymin"],
    ymx = grid_info$bbox["ymax"],
    nrows = grid_info$dimensions[1] * 10,
    ncols = grid_info$dimensions[2] * 10,
    crs = lat_long_proj
    )
  with_points <- raster::rasterize(point_population, raster100, field = "pop", fun = "count")
  with_points
}



add_area_section <- function(pops) {
  axis <- function(row_col) {
    function(name) {
      # Names are of the form "M0090S090".
      sapply(strsplit(name, "[MS]")[[1]][1 + row_col], function(x) strtoi(x, base = 10))
    }
  }
  pops["area"] <- sapply(pops[["secId"]], axis(1))
  pops["sec"] <- sapply(pops[["secId"]], axis(2))
  pops
}


#' Given an HRSL raster, convert it into points in space.
#'
#' @param hsrl_raster This is a raster read from the HRSL geotiff.
#' @return An sf points dataset with populations on the points.
#' @export
hrsl_points <- function(hrsl_raster) {
  # Make 3 columns, (x, y, population)
  hrsl_xym <- raster::rasterToPoints(hrsl_raster)
  # Make Points with one feature, the population.
  hrsl_sf <- sf::st_as_sf(x = data.frame(hrsl_xym), coords = 1:2)
  sf::st_crs(hrsl_sf) <- sp::CRS("+init=epsg:4326")
  hrsl_sf
}


#' Given spatial points of population, assign them to shapes.
#'
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
