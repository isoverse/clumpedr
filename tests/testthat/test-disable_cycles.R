context("Disabled Cycles")

test_that("find_bad_cycles works", {
  bad_cyc <- find_bad_cycles(isoreader::iso_get_raw_data(standards))
   # is it a tibble?
  expect_is(bad_cyc, "tbl_df")
  # do all the new column names exist?
  expect_true("v44_low" %in% colnames(bad_cyc))
  expect_true("v44_high" %in% colnames(bad_cyc))
  expect_true("v44_diff" %in% colnames(bad_cyc))
  expect_true("v44_drop" %in% colnames(bad_cyc))
  expect_true("has_drop" %in% colnames(bad_cyc))
  expect_true("cycle_drop" %in% colnames(bad_cyc))
  expect_true("drop_before" %in% colnames(bad_cyc))
  # are they of the right type?
  expect_is(bad_cyc %>% pluck("v44_low"), "logical")
  expect_is(bad_cyc %>% pluck("v44_high"), "logical")
  expect_is(bad_cyc %>% pluck("v44_diff"), "numeric")
  expect_is(bad_cyc %>% pluck("v44_drop"), "logical")
  expect_is(bad_cyc %>% pluck("has_drop"), "logical")
  expect_is(bad_cyc %>% pluck("drop_before"), "logical")
  ## expect_equal(bad_cyc %>%
  ##                filter(file_id == "180814_75_IAM_2_ETH-3.did", cycle == ))
})

test_that("plot_disabled_cycles works", {
  expect_is(plot_disabled_cycles(find_bad_cycles(isoreader::iso_get_raw_data(standards))), "ggplot")
})
