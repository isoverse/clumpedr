test_that("append_expected_values works", {
  expected_test <- standards %>%
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
    append_expected_values()
  expect_is(expected_test, "tbl_df")
  expect_true("expected_D47" %in% colnames(expected_test))
})

test_that("calculate_etf works", {
  etf_test <- standards %>%
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
    append_expected_values() %>%
    calculate_etf()
  expect_is(etf_test, "tbl_df")
  expect_true("etf" %in% colnames(etf_test))
  expect_true("etf_coefs" %in% colnames(etf_test))
  expect_true("intercept" %in% colnames(etf_test))
  expect_true("slope" %in% colnames(etf_test))
})


test_that("apply_etf works", {
  etf_apply_test <- standards %>%
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
    append_expected_values() %>%
    calculate_etf() %>%
    apply_etf()
  expect_is(etf_apply_test, "tbl_df")
  expect_true("D47_etf" %in% colnames(etf_apply_test))
})


test_that("plot_etf works", {
  etf_apply_test <- standards %>%
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
    append_expected_values() %>%
    calculate_etf() %>%
    apply_etf() %>%
    plot_etf()
  expect_is(etf_apply_test, "ggplot")
})


test_that("empirical transfer function wrapper works", {
  expect_is(standards %>%
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
              empirical_transfer_function(genplot=FALSE),
            "tbl_df")
})
