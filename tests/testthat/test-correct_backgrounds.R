test_that("background correction", {
  expect_is(correct_backgrounds(iso_get_raw_data(standards), factor=1), "tbl_df")
})
