## context("Collapsing cycles")
## test_that("collapsing the cycles", {
##   collapse_test <- standards %>%
##     isoreader::iso_get_raw_data() %>%
##     disable_cycles() %>%
##     spread_match() %>%
##     append_ref_deltas(standards) %>%
##     delta_values() %>%
##     collapse_cycles(cols = c(d18O_PDB, d13C_PDB))
##   expect_is(collapse_test, "tbl_df")
##   # do the summary columns exist?
##   expect_true(all(expand.grid(c("d18O_PDB", "d13C_PDB"),
##                               c("mean", "sd", "sem", "cl")) %>%
##                     tidyr::unite(col = "new") %>%
##                     pull(new) %in% colnames(collapse_test)))
## })

## test_that("nest_cycle_data works", {
  ## nest_test <- standards %>%
  ##   isoreader::iso_get_raw_data() %>%
  ##   disable_cycles() %>%
  ##   spread_match() %>%
  ##   # correct backgrounds
  ##   append_ref_deltas(standards) %>%
  ##   delta_values(genplot=FALSE) %>%
  ##   abundance_ratios(s44, s45_bg, s46_bg, s47_bg, s48_bg, s49_bg) %>%
  ##   abundance_ratios(r44, r45_bg, r46_bg, r47_bg, r48_bg, r49_bg,
  ##                                  R45_wg, R46_wg, R47_wg, R48_wg, R49_wg) %>%
  ##   little_deltas() %>%
  ##   bulk_and_clumping_deltas() %>%
  ##   nest_cycle_data()
  ## expect_is(nest_test, "tbl_df")
## })
