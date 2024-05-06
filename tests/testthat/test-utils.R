test_that("check_quiet() works", {
  expect_equal(check_quiet(TRUE), TRUE)
  expect_equal(check_quiet(FALSE), FALSE)
  expect_error(check_quiet("hi"))
  expect_error(check_quiet(NA))
})
