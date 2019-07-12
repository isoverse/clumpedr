test_that("adding file information works", {
  expect_is(standards %>%
    iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
      collapse_cycles() %>%
      add_info(iso_get_file_info(standards)), "tbl_df")
})
