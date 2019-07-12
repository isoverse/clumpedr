test_that("acid_fractionation works", {
  aff_test <- standards %>%
    clean_did_info("MOTU") %>%
    iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
    collapse_cycles() %>%
    add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    unnest(cycle_data) %>%
    find_outliers() %>%
    empirical_transfer_function(genplot=FALSE) %>%
    acid_fractionation()
  expect_is(aff_test, "tbl_df")
  expect_true("D47_final" %in% colnames(aff_test))
})
