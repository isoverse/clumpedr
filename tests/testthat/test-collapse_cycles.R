context("Collapsing cycles")
test_that("collapsing the cycles", {
  collapse_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    disable_cycles(genplot = FALSE) %>%
    spread_match() %>%
    filter(cycle != 0) %>%
    mutate(outlier_cycle = s44_outlier_cycle | r44_outlier_cycle) %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
    collapse_cycles(cols = c(d18O_PDB, d13C_PDB))
  expect_is(collapse_test, "tbl_df")
  # do the summary columns exist?
  expect_true(all(expand.grid(c("d18O_PDB", "d13C_PDB"),
                              c("mean", "sd", "sem", "cl")) %>%
                    tidyr::unite(col = "new") %>%
                    pull(new) %in% colnames(collapse_test)))
})
