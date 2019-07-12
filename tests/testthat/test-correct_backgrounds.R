context("Background Correction")
test_that("correct_backgrounds works", {
  expect_is(correct_backgrounds(isoreader::iso_get_raw_data(standards), factor=1), "tbl_df")
})
