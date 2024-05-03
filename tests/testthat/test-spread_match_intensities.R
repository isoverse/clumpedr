test_that("spread_intensities works", {
  spread_test <- standards %>%
    isoreader::iso_get_raw_data(include_file_info = "Analysis") %>%
    mutate(min = 1500, max = 50000, fac = 3) |>
    find_bad_cycles(min = "min", max = "max", fac = "fac") %>%
    spread_intensities()
  # it's a tibble
  expect_s3_class(spread_test, "tbl_df")
  # it has the right columns
  expect_true("r44" %in% colnames(spread_test))
  expect_true("r45" %in% colnames(spread_test))
  expect_true("r46" %in% colnames(spread_test))
  expect_true("r47" %in% colnames(spread_test))
  expect_true("r48" %in% colnames(spread_test))
  expect_true("r49" %in% colnames(spread_test))
  expect_true("r54" %in% colnames(spread_test))
  expect_true("s44" %in% colnames(spread_test))
  expect_true("s45" %in% colnames(spread_test))
  expect_true("s46" %in% colnames(spread_test))
  expect_true("s47" %in% colnames(spread_test))
  expect_true("s48" %in% colnames(spread_test))
  expect_true("s49" %in% colnames(spread_test))
  expect_true("s54" %in% colnames(spread_test))
  expect_true("v44_low_standard" %in% colnames(spread_test))
  expect_true("v44_low_sample" %in% colnames(spread_test))
  expect_true("v44_high_standard" %in% colnames(spread_test))
  expect_true("v44_high_sample" %in% colnames(spread_test))
  expect_true("v44_drop_standard" %in% colnames(spread_test))
  expect_true("v44_drop_sample" %in% colnames(spread_test))
  expect_true("drop_before_standard" %in% colnames(spread_test))
  expect_true("drop_before_sample" %in% colnames(spread_test))
  expect_true("has_drop_standard" %in% colnames(spread_test))
  expect_true("has_drop_sample" %in% colnames(spread_test))
  # for now, I'll just believe that they're the right type.
})

test_that("match_intensities works", {
  match_test <- standards %>%
    isoreader::iso_get_raw_data(include_file_info = "Analysis") %>%
    mutate(min = 1500, max = 50000, fac = 3) |>
    find_bad_cycles(min = "min", max = "max", fac = "fac") %>%
    spread_intensities() %>%
    match_intensities()
  # it's a tibble
  expect_s3_class(match_test, "tbl_df")
  # it has the right columns
  expect_true("outlier_cycle" %in% colnames(match_test))
  # they are of the right type
  expect_type(match_test %>% pluck("outlier_cycle"), "logical")
})

test_that("spread_match wrapper works", {
  expect_s3_class(standards %>%
              isoreader::iso_get_raw_data(include_file_info = "Analysis") %>%
              mutate(dis_min = 500, dis_max = 50000, dis_fac = 3) |>
              find_bad_cycles(min = "dis_min", max = "dis_max", fac = "dis_fac", relative_to = "init") |>
              spread_match(), "tbl_df")
})
