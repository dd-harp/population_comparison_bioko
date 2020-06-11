make_point_data <- function() {
  # These are x, y, population count.
  pts <- matrix(
    c(
      2, 2, 5.0,  # Right in the middle is 10.
      2.1, 2, 5.0,
      2, 2.7, 8.0,  # On the side of the admin is 8.
      2, 3.7, 14.0 # Off the edge of the admin is 14.
    ),
    ncol = 3, byrow = TRUE
  )
  stpts <- lapply(
    1:nrow(pts),
    function(idx) sf::st_point(c(pts[idx, 1], pts[idx, 2]))
    )
  sf::st_sf(
    sf::st_sfc(stpts),
    pop = pts[1:4, 3],
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
}


#' Make a single rectangle. Width 1 to 5. Height 1 to 3.
make_admin <- function() {
  ll <- c(1, 1)
  ur <- c(5, 3)  # first coordinate is longitude so it's x, y.
  admin_poly <- sf::st_polygon(list(matrix(
    c(ll[1], ll[2],  ur[1], ll[2],  ur[1], ur[2],  ll[1], ur[2],  ll[1], ll[2]),
    ncol = 2, byrow = TRUE
    )))
  sf::st_sf(
    sf::st_sfc(admin_poly),
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )
}


#' It's a vertical set of 5 cells from 0 to 6.
#' Width is (1, 3). Height is 0 to 4.
make_grid <- function() {
  raster::raster(
    nrows = 5, ncols = 1, xmn = 1, xmx = 3, ymn = 0, ymx = 4,
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
    vals = seq(5)
    )
}


plot_one <- function() {
  adm <- make_admin()
  grid <- make_grid()
  polys <- raster::rasterToPolygons(grid)
  points <- make_point_data()
  tmap::tm_shape(
    adm, bbox = matrix(c(0, 0, 6, 6), nrow = 2)) +
    tmap::tm_polygons() + tmap::tm_shape(polys) + tmap::tm_polygons() +
    tmap::tm_shape(points) + tmap::tm_bubbles(size = 1, col = "pop")
}


# Use lat 1.4N and lon 32E.

test_that("simple raster points placed well", {
  admin <- make_admin()
  grid <- make_grid()
  gps <- make_point_data()
  gridded <- geolocated_on_grid(grid, gps, admin)
  vals <- raster::getValues(gridded)
  expect_equal(vals[1], 14)
  expect_equal(vals[2], 8)
  expect_equal(vals[3], 10)
  expect_equal(vals[4], 0)
  expect_true(is.na(vals[5]))
})
