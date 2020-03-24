test_that("the area is a square kilometer", {
  dims <- c(1000, 1000)  # row, column
  range <- c(2000, 2000)  # width, height
  to_coord <- function(range, dim) {
    nn <- 0:dim * range / dim
    0.5 * (nn[1:length(nn) - 1] + nn[2:length(nn)])
  }
  xvals <- to_coord(range[1], dims[2])
  yvals <- to_coord(range[2], dims[1])
  pixel_area <- (xvals[2] - xvals[1]) * (yvals[2] - yvals[1])
  mat <- matrix(0.00, nrow = dims[1], ncol = dims[2])
  peak_value <- 50
  mat[dims[1] %/% 2, dims[2] %/% 2] <- peak_value

  fake_im <- spatstat::im(mat, xrange = c(0, range[1]), yrange = c(0, range[2]))
  fake_raster <- raster::raster(maptools::as.SpatialGridDataFrame.im(fake_im))
  blurred <- density_from_disc(fake_raster)

  pixel_area_covered <- sum(values(blurred) > 0.0001, na.rm = TRUE) * pixel_area
  expect_true(abs(pixel_area_covered - 10^6) / 10^6 < 1e-2)
  # peak_value * pixel_area = max value * 1 km
  # max value = peak value * pixel area / 1 km
  expected_max <- peak_value * pixel_area / 10^6
  expect_true(abs(max(values(blurred), na.rm = TRUE) - expected_max) / expected_max < 1e-2)
})
