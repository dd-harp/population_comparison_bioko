
test_that("pareto fraction is within 10%", {
  side <- 10^6
  quantiles <- seq(0, 1, 1 / side)[1:side]  # Cut off the value 1.
  pops <- VGAM::qpareto(quantiles, scale = 1, shape = log(5, 4))
  fraction <- pareto_fraction(pops)
  expect_true(abs(fraction - .2) / .2 < .1, paste("fraction is", fraction))
})


test_that("pareto is correct for small counts", {
  val <- c(rep(0.2 / 8, 8), rep(0.8 / 2, 2))
  fraction <- pareto_fraction(val)
  expect_true(abs(fraction - 0.2) / 0.2 < 1e-5, paste("fraction is", fraction))
})


test_that("pareto ignores NA values", {
  val <- c(rep(NA, 10), rep(0.2 / 8, 8), rep(0.8 / 2, 2))
  fraction <- pareto_fraction(val)
  expect_true(abs(fraction - 0.2) / 0.2 < 1e-5, paste("fraction is", fraction))
})


test_that("urban fraction ignores NA", {
  frac <- urban_fraction(c(1001, 327, 68, NA, 4))
  expect_true(abs(frac - 0.25) < 1e-7, paste("fraction is", frac))
})
