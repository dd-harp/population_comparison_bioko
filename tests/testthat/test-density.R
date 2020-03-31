relative_error <- function(observed, expected) {
  abs(observed - expected) / expected
}


test_that("density_from_disc average value correct for uniform population", {
  dims <- c(100, 100)  # row, column
  range <- c(2000, 2000)  # width, height

  dimension_coordinates <- function(range, dim) {
    nn <- 0:dim * range / dim
    0.5 * (nn[1:(length(nn) - 1)] + nn[2:length(nn)])
  }

  # xvals need columns, yvals need rows, so they are switched.
  xvals <- dimension_coordinates(range[1], dims[2])
  yvals <- dimension_coordinates(range[2], dims[1])
  pixel_area <- (xvals[2] - xvals[1]) * (yvals[2] - yvals[1])
  expected_density <- 500 * pixel_area / 10^6
  mat <- matrix(expected_density, nrow = dims[1], ncol = dims[2])

  fake_im <- spatstat::im(mat, xcol = xvals, yrow = yvals)
  fake_raster <- raster::raster(maptools::as.SpatialGridDataFrame.im(fake_im))
  blurred <- density_from_disc(fake_raster)

  blur_mat <- as.matrix(blurred)
  observed_density <- blur_mat[dims[1] %/% 2, dims[2] %/% 2]
  one_percent <- 0.01
  expect_lt(relative_error(observed_density, expected_density), one_percent)
})


test_that("the area is a square kilometer", {
  dims <- c(1000, 1000)  # row, column
  range <- c(2000, 2000)  # width, height

  dimension_coordinates <- function(range, dim) {
    nn <- 0:dim * range / dim
    0.5 * (nn[1:(length(nn) - 1)] + nn[2:length(nn)])
  }

  # xvals need columns, yvals need rows, so they are switched.
  xvals <- dimension_coordinates(range[1], dims[2])
  yvals <- dimension_coordinates(range[2], dims[1])
  pixel_area <- (xvals[2] - xvals[1]) * (yvals[2] - yvals[1])
  mat <- matrix(0.0, nrow = dims[1], ncol = dims[2])
  peak_value <- 50
  mat[dims[1] %/% 2, dims[2] %/% 2] <- peak_value

  fake_im <- spatstat::im(mat, xcol = xvals, yrow = yvals)
  fake_raster <- raster::raster(maptools::as.SpatialGridDataFrame.im(fake_im))
  blurred <- density_from_disc(fake_raster)

  # The single pixel is spread out into a square kilometer.
  pixel_area_covered <- sum(raster::values(blurred) > 0.0001, na.rm = TRUE) * pixel_area
  square_km <- 10^6
  one_percent <- 1e-2
  expect_lt(relative_error(pixel_area_covered, square_km), one_percent)

  # The values in that square kilometer add up to the single pixel in the center.
  expect_lt(
    relative_error(sum(raster::values(blurred), na.rm = TRUE), peak_value),
    one_percent
  )

  # The max value is the same as the average value, so it's a disc.
  # peak_value * pixel_area = max value * 1 km2
  # max value = peak value * pixel area / 1 km2
  expected_max <- peak_value * pixel_area / square_km
  observed_max <- max(raster::values(blurred), na.rm = TRUE)
  expect_lt(relative_error(observed_max, expected_max), one_percent)
})




test_that("density_from_disc NA values remain", {
  dims <- c(100, 100)  # row, column
  range <- c(2000, 2000)  # width, height

  dimension_coordinates <- function(range, dim) {
    nn <- 0:dim * range / dim
    0.5 * (nn[1:(length(nn) - 1)] + nn[2:length(nn)])
  }

  # xvals need columns, yvals need rows, so they are switched.
  xvals <- dimension_coordinates(range[1], dims[2])
  yvals <- dimension_coordinates(range[2], dims[1])
  pixel_area <- (xvals[2] - xvals[1]) * (yvals[2] - yvals[1])
  mat <- matrix(0.0, nrow = dims[1], ncol = dims[2])
  peak_value <- 50
  middle <- c(dims[1] %/% 2, dims[2] %/% 2)
  mat[middle[1], middle[2]] <- peak_value
  expect_equal(sum(is.na(mat)), 0)
  mat[(middle[1] + 1):dims[1], 1:dims[2]] <- NA
  na_expected <- sum(is.na(mat))
  expect_gt(na_expected, .4 * dims[1] * dims[2])

  fake_im <- spatstat::im(mat, xcol = xvals, yrow = yvals)
  fake_raster <- raster::raster(maptools::as.SpatialGridDataFrame.im(fake_im))
  blurred <- density_from_disc(fake_raster)

  # The result should have the same number of NA values.
  na_observed <- sum(is.na(raster::values(blurred)))
  expect_equal(na_observed, na_expected)
})



test_that("density_from_disc accounts for NA borders", {
  dims <- c(1000, 1000)  # row, column
  range <- c(2000, 2000)  # width, height

  dimension_coordinates <- function(range, dim) {
    nn <- 0:dim * range / dim
    0.5 * (nn[1:(length(nn) - 1)] + nn[2:length(nn)])
  }

  # xvals need columns, yvals need rows, so they are switched.
  xvals <- dimension_coordinates(range[1], dims[2])
  yvals <- dimension_coordinates(range[2], dims[1])
  pixel_area <- (xvals[2] - xvals[1]) * (yvals[2] - yvals[1])
  mat <- matrix(0.0, nrow = dims[1], ncol = dims[2])

  # Create a single peak right next to a border of NA values.
  peak_value <- 50
  middle <- c(dims[1] %/% 2, dims[2] %/% 2)
  mat[middle[1], middle[2]] <- peak_value
  mat[(middle[1] + 1):dims[1], 1:dims[2]] <- NA

  fake_im <- spatstat::im(mat, xcol = xvals, yrow = yvals)
  fake_raster <- raster::raster(maptools::as.SpatialGridDataFrame.im(fake_im))
  blurred <- density_from_disc(fake_raster)

  # The single pixel is spread out into roughly half a square kilometer.
  pixel_area_covered <- sum(raster::values(blurred) > 0.0001, na.rm = TRUE) * pixel_area
  square_km <- 10^6
  one_percent <- 1e-2
  expect_lt(relative_error(pixel_area_covered, 0.5 * square_km), 5 * one_percent)

  # The values in that square kilometer add up to the single pixel in the center.
  expect_lt(
    relative_error(sum(raster::values(blurred), na.rm = TRUE), peak_value),
    one_percent
  )

  # The max value is the same as the average value, so it's a disc.
  # peak_value * pixel_area = max value * 1 km2
  # max value = peak value * pixel area / 1 km2
  expected_max <- peak_value * pixel_area / square_km
  observed_max <- max(raster::values(blurred), na.rm = TRUE)
  expect_lt(relative_error(observed_max, expected_max), 5 * one_percent)
})
