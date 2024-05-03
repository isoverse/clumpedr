test_that("summarise_outliers() works", {
  expect_equal(summarise_outlier(.data = tibble()), tibble(file_id = character()))
  out <- tibble(expand.grid(outlier_hoi = c(TRUE, FALSE), outlier_doei = c(TRUE, FALSE))) |>
    summarise_outlier()
  expect_true("outlier" %in% colnames(out))
  expect_equal(out$outlier, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("find_internal_sd_outlier() works", {
  tst <- tibble(file_id = sample(letters[1:3], replace = TRUE, size = 10), D47_raw = rnorm(10)) |>
    find_internal_sd_outlier(1)
  expect_true("outlier_internal_sd" %in% colnames(tst))
})

test_that("find_session_outlier() works", {
  # TODO
})

test_that("find_session_id1_outlier() works", {
  # TODO
})

test_that("find_outliers() works", {
  # this is the wrapper
  # TODO update these tests, work from minimal tibble so that other tests take care of the rest
  outliers_test <- tibble(outlier_cycle = NA, outlier_init = FALSE, outlier_session = TRUE, outlier = TRUE)
  # this is a bit silly
  expect_s3_class(outliers_test, "tbl_df")
  expect_true("outlier_cycle" %in% colnames(outliers_test))
  expect_true("outlier_init" %in% colnames(outliers_test))
  expect_true("outlier_session" %in% colnames(outliers_test))
  expect_true("outlier" %in% colnames(outliers_test))
})
