test_that("abundance_ratios works", {
  abundance_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    abundance_ratios()
  expect_type(abundance_test, "tbl_df")
  expect_true("R45" %in% colnames(abundance_test))
  expect_true("R46" %in% colnames(abundance_test))
  expect_true("R47" %in% colnames(abundance_test))
  expect_true("R48" %in% colnames(abundance_test))
  expect_true("R49" %in% colnames(abundance_test))
})

test_that("little_deltas works", {
  deltas_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    abundance_ratios() %>%
    abundance_ratios(i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48,
                     i49 = r49, R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                     R48 = R48_wg, R49 = R49_wg) %>%
    little_deltas()
  expect_type(deltas_test, "tbl_df")
  expect_true("d45" %in% colnames(deltas_test))
  expect_true("d46" %in% colnames(deltas_test))
  expect_true("d47" %in% colnames(deltas_test))
  expect_true("d48" %in% colnames(deltas_test))
  expect_true("d49" %in% colnames(deltas_test))
})

test_that("bulk_and_clumping_deltas works", {
  bulk_clump_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    abundance_ratios() %>%
    abundance_ratios(i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48,
                     i49 = r49, R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                     R48 = R48_wg, R49 = R49_wg) %>%
    little_deltas() %>%
    bulk_and_clumping_deltas()
  expect_type(bulk_clump_test, "tbl_df")
  expect_true("D47_raw" %in% colnames(bulk_clump_test))
  expect_true("D48_raw" %in% colnames(bulk_clump_test))
  expect_true("D49_raw" %in% colnames(bulk_clump_test))
  expect_true("param_49" %in% colnames(bulk_clump_test))
})

test_that("delta_values wrapper works", {
  expect_type(standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(), "tbl_df")
})
