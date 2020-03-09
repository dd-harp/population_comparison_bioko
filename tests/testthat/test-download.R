test_that("filenames_to_description makes a dataframe", {
  result_df <- filenames_to_description(c(
    "HRSL1000_BIMEP.tif", "LandScan100_LandScan.tif", "WorldPop100_BIMEP.tif"
  ))
  expect_equal(nrow(result_df), 3)
  expect_equal(result_df[1, "filename"], "HRSL1000_BIMEP.tif")
  expect_equal(result_df[1, "name"], "BIMEP on HRSL")
  expect_equal(result_df[1, "resolution"], 1000)
  expect_equal(result_df[1, "source"], "BIMEP")
  expect_equal(result_df[1, "grid"], "HRSL")
})
