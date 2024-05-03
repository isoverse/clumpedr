test_that("correct_backgrounds works", {
  expect_s3_class(correct_backgrounds(isoreader::iso_get_raw_data(standards, include_file_info = "Analysis"), factor=1), "tbl_df")
})
