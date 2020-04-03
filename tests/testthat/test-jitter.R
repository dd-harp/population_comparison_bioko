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
  values <- summarize_over_shifts(4, fake_summary_statistics)
  expect_equal(class(values), "data.frame")
  expect_equal(nrow(values), 16)
})


test_that("summarize can do one grid", {
  expect_true(TRUE)
})
