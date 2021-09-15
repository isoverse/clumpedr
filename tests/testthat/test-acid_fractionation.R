context("Acid Fractionation Factor")
test_that("acid_fractionation works", {
  dat <- tibble(x = 1:10, y = 11:20)
  dat_test <- acid_fractionation(dat, aff = .5, D47 = y, D47_out = z)
  expect_true("z" %in% colnames(dat_test))
  expect_equal(dat_test$z, 11:20 + .5)
})
