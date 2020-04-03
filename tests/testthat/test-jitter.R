fake_summary_statistics <- function(pixel_fraction) {
  list(
    total = 2400,
    side_meters = 90,
    maximum = 12000,
    empty_percent = 88,
    # Use the fit values for pareto fraction so they are more stable.
    pareto_fraction = 1/24,
    urban_num = 240,
    urban_den = 2400,
    urban_raw = 0.1,
    urban_fit_num = 240,
    urban_fit_den = 2400,
    urban_fit = .1,
    na_percent = 0.23
  )
}


test_that("the summarize loop properly gathers its outputs", {
  tempfile <- "test.csv"
  if (file.exists(tempfile)) file.remove(tempfile)
  values <- summarize_over_shifts(4, fake_summary_statistics)
  expect_equal(class(values), "data.frame")
  expect_equal(nrow(values), 16)
  write.csv(values, tempfile, quote = FALSE, row.names = FALSE)
})


test_that("summarize can do one grid", {
  skip("taking longer")
  local_directory <- rprojroot::find_package_root_file("inst/extdata")
  geolocated <- read_bimep_point_data(local_directory)
  admin_sf <- sf::st_read(fs::path(local_directory, "source", "bioko.shp"))
  base_grid <- read_landscan(local_directory)
  grid <- clean_grid(base_grid, admin_sf)
  pixel_fraction <- c(0.1, 0.1)
  urban_cutoff <- 1000
  summary <- summarize_shifted_alignment(geolocated, admin_sf, grid, pixel_fraction, urban_cutoff)
  expect_gt(length(summary), 5)
  expect_gt(summary$total, 10000)
  expect_gt(summary$maximum, 1000)
  expect_gt(summary$empty_percent, 20)
})
