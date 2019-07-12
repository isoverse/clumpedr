test_that("extracted reference gas delta values correctly", {
  expect_is(get_ref_delta(standards), "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(get_ref_delta(standards)))
  expect_true("d18O_PDBCO2_wg" %in% colnames(get_ref_delta(standards)))
})

test_that("appending reference gas delta values works", {
  ref_delta_test <- append_ref_deltas(match_intensities(spread_intensities(find_bad_cycles(iso_get_raw_data(standards)))), standards)
  expect_is(ref_delta_test, "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(ref_delta_test))
  expect_true("d18O_PDBCO2_wg" %in% colnames(ref_delta_test))
})
