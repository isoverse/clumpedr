test_that("append_expected_values works", {
  expect_error(append_expected_values(standards, std_names = "hoi", std_values = c(5, 4)),
               "std_names should be of equal length to std_values.")
  expected_test <- standards %>%
    clean_did_info("MOTU") %>%
    iso_get_raw_data() %>%
    add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    append_expected_values()
  expect_type(expected_test, "tbl_df")
  expect_true("expected_D47" %in% colnames(expected_test))
})

test_that("calculate_etf works", {
  etf_test <- standards %>%
    clean_did_info("MOTU") %>%
    iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values() %>%
    collapse_cycles(d18O_PDBCO2, d13C_PDB, D47_raw) %>%
    add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    unnest(cycle_data) %>%
    find_outliers() %>%
    append_expected_values() %>%
    calculate_etf()
  expect_type(etf_test, "tbl_df")
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
    delta_values() %>%
    collapse_cycles(d18O_PDBCO2, d13C_PDB, D47_raw) %>%
    add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
    unnest(cycle_data) %>%
    find_outliers() %>%
    append_expected_values() %>%
    calculate_etf() %>%
    apply_etf()
  expect_type(etf_apply_test, "tbl_df")
  expect_true("D47_etf" %in% colnames(etf_apply_test))
})


test_that("empirical transfer function wrapper works", {
  expect_type(standards %>%
              clean_did_info("MOTU") %>%
              iso_get_raw_data() %>%
              find_bad_cycles() %>%
              spread_match() %>%
              append_ref_deltas(standards) %>%
              delta_values() %>%
              collapse_cycles(d18O_PDBCO2, d13C_PDB, D47_raw) %>%
              add_info(iso_get_file_info(clean_did_info(standards, "MOTU"))) %>%
              unnest(cycle_data) %>%
              find_outliers() %>%
              empirical_transfer_function(),
            "tbl_df")
})
