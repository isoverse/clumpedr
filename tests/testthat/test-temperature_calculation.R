test_that("temperature_calculation works", {
  expect_snapshot(temp_test <- temperature_calculation(.data = tibble(D47_final = seq(0.25, 1, length.out = 10), slope = 0.333, intercept = 0.22)))
  expect_s3_class(temp_test, "tbl_df")
  expect_true("temperature" %in% colnames(temp_test))
})
