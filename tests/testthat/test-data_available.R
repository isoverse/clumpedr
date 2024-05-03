test_that("data are available", {
  expect_s3_class(eth3, "iso_file")
  expect_s3_class(standards, "iso_file_list")
})
