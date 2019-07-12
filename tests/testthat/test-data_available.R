context("Test Data")
test_that("data are available", {
  expect_is(eth3, "iso_file")
  expect_is(standards, "iso_file_list")
})
