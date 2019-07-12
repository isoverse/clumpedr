context("Appending Reference gas \u03b4 values")

test_that("get_ref_delta works", {
  expect_is(get_ref_delta(standards), "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(get_ref_delta(standards)))
  expect_true("d18O_PDBCO2_wg" %in% colnames(get_ref_delta(standards)))
})

test_that("append_ref_deltas works", {
  ref_delta_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards)
  expect_is(ref_delta_test, "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(ref_delta_test))
  expect_true("d18O_PDBCO2_wg" %in% colnames(ref_delta_test))
})
