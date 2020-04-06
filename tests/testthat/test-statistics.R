relerr <- function(observed, expected) {
  ifelse(abs(expected) > 1e-5, abs((observed - expected) / expected), observed)
}


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
  urban <- 1000 / 9
  sub <- 0.9 * urban
  place_raster <- raster::raster(
    matrix(c(1.1 * urban, sub, sub, NA, sub, rep(NA, 4)), nrow = 3, ncol = 3),
    xmn = 0, xmx = 1000,
    ymn = 0, ymx = 1000
    )
  numden <- urban_fraction_population(place_raster, 1000)
  frac <- numden[1] / numden[2]
  expect_true(abs(frac - 0.25) < 1e-7, paste("fraction is", frac))
})


test_that("truth table matches simple expectations", {
  trials <- list(
    list(a = c(1), b = c(1), TP = 1, FP = 0, FN = 0, TN = 0),
    list(a = c(0), b = c(1), TP = 0, FP = 1, FN = 0, TN = 0),
    list(a = c(1), b = c(0), TP = 0, FP = 0, FN = 1, TN = 0),
    list(a = c(0), b = c(0), TP = 0, FP = 0, FN = 0, TN = 1),
    list(a = c(1, 1, 1), b = c(1, 0, 0), TP = 1, FP = 0, FN = 2, TN = 0)
  )
  condition <- function(x) {x == 1}
  for (trial_idx in 1:length(trials)) {
    with(trials[[trial_idx]], {
      result <- truth_table(condition, a, b)
      expect_true(result$TP == TP, trial_idx)
      expect_true(result$FP == FP, trial_idx)
      expect_true(result$TN == TN, trial_idx)
      expect_true(result$FN == FN, trial_idx)
    })
  }
})



test_that("accuracy table matches simple expectations", {
  trials <- list(
    list(t = list(TP = 1, FP = 0, FN = 0, TN = 0), acc = 1, sens = 1, spec = 0, ppv = 1, npv = 0),
    list(t = list(TP = 0, FP = 1, FN = 0, TN = 0), acc = 0, sens = 0, spec = 0, ppv = 0, npv = 0),
    list(t = list(TP = 0, FP = 0, FN = 1, TN = 0), acc = 0, sens = 0, spec = 0, ppv = 0, npv = 0),
    list(t = list(TP = 0, FP = 0, FN = 0, TN = 1), acc = 1, sens = 0, spec = 1, ppv = 0, npv = 1),
    list(t = list(TP = 1, FP = 0, FN = 0, TN = 1), acc = 1, sens = 1, spec = 1, ppv = 1, npv = 1),
    list(t = list(TP = 1, FP = 1, FN = 1, TN = 1), acc = .5, sens = .5, spec = .5, ppv = .5, npv = .5)
  )
  condition <- function(x) {x == 1}
  eps <- 1e-5
  for (trial_idx in 1:length(trials)) {
    with(trials[[trial_idx]], {
      result <- accuracy_profile(t)
      expect_lt(relerr(result$acc, acc), eps, trial_idx)
      expect_lt(relerr(result$sens, sens), eps, trial_idx)
      expect_lt(relerr(result$spec, spec), eps, trial_idx)
      expect_lt(relerr(result$ppv, ppv), eps, trial_idx)
      expect_lt(relerr(result$npv, npv), eps, trial_idx)
    })
  }
})
