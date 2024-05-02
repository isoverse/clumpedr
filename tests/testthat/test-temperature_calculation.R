test_that("temperature_calculation works", {
  expect_warning(temp_test <- standards %>%
    clean_did_info("MOTU") %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values() %>%
    collapse_cycles() %>%
    add_info(isoreader::iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    unnest(cycle_data) %>%
    find_outliers() %>%
    empirical_transfer_function() %>%
    acid_fractionation() %>%
    temperature_calculation(), "NaNs produced")
  expect_type(temp_test, "tbl_df")
  expect_true("temperature" %in% colnames(temp_test))
})
