context("Collapsing cycles")
test_that("collapsing the cycles", {
  collapse_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
    collapse_cycles(d45:d49, d18O_PDBCO2, d13C_PDB, D47_raw)
  expect_is(collapse_test, "tbl_df")
})
