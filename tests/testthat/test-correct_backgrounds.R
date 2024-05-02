test_that("correct_backgrounds works", {
  expect_type(correct_backgrounds(isoreader::iso_get_raw_data(standards), factor=1), "tbl_df")
})
