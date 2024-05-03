test_that("correct_backgrounds() works", {
  # NOTE: this is only the half-cup correction!
  expect_equal(
    correct_backgrounds(.data = tibble(v47.mV = seq(0, 1, length.out = 10), v54.mV = seq(.1, 0, length.out = 10)), factor = 1),
    structure(list(v47.mV = c(-0.1, 0.0222222222222222, 0.144444444444444, 0.266666666666667, 0.388888888888889, 0.511111111111111, 0.633333333333333, 0.755555555555555, 0.877777777777778, 1),
                   v54.mV = c(0.1, 0.0888888888888889, 0.0777777777777778, 0.0666666666666667, 0.0555555555555556, 0.0444444444444444, 0.0333333333333333, 0.0222222222222222, 0.0111111111111111, 0)),
              row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"))

  )
})
