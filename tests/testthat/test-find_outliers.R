test_that("finding outliers works", {
  outliers_test <- standards %>%
    clean_did_info("MOTU") %>%
    iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
    collapse_cycles() %>%
    add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    unnest(cycle_data) %>%
    find_outliers()
  expect_is(outliers_test, "tbl_df")
  expect_true("outlier_cycle" %in% colnames(outliers_test))
  expect_true("outlier_init" %in% colnames(outliers_test))
  expect_true("outlier_session" %in% colnames(outliers_test))
  expect_true("outlier" %in% colnames(outliers_test))
})
