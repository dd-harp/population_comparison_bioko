test_that("filenames_to_description makes a dataframe", {
  result_df <- filenames_to_description(c(
    "HRSL_coarse_BIMEP.tif", "LandScan_fine_LandScan.tif", "WorldPop_fine_BIMEP.tif"
  ))
  expect_equal(nrow(result_df), 3)
  expect_equal(result_df[1, "filename"], "HRSL_coarse_BIMEP.tif")
  expect_equal(result_df[1, "name"], "BIMEP on HRSL coarse")
  expect_equal(result_df[1, "resolution"], "coarse")
  expect_equal(result_df[1, "source"], "BIMEP")
  expect_equal(result_df[1, "grid"], "HRSL")
})
