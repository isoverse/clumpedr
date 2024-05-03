test_that("get_ref_delta() works", {
  expect_s3_class(get_ref_delta(standards), "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(get_ref_delta(standards)))
  expect_true("d18O_PDBCO2_wg" %in% colnames(get_ref_delta(standards)))
})

test_that("append_ref_deltas() works", {
  expect_equal(append_ref_deltas(.data = tibble()), tibble(file_id = character()))
  expect_error(append_ref_deltas(), "argument \".data\" is missing, with no default",
               fixed = TRUE)
  expect_error(append_ref_deltas(.data = "hoi"), "must", fixed = TRUE)
  expect_error(append_ref_deltas(.data = tibble(a = 1:10, b = 11:20), did = "hoi"), "must", fixed = TRUE)
  expect_error(append_ref_deltas(.data = tibble(a = 1:10, b = 11:20), did = standards), "must", fixed = TRUE)
  # this one might be too complex to rewrite to not rely on any other functions from our package
  ref_delta_test <- standards %>%
    isoreader::iso_get_raw_data(include_file_info = "Analysis") %>%
    mutate(dis_min = 500, dis_max = 50000, dis_fac = 3) %>%
    find_bad_cycles(min = "dis_min", max = "dis_max", fac = "dis_fac", relative_to = "init") %>%
    spread_match() %>%
    append_ref_deltas(standards)
  expect_s3_class(ref_delta_test, "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(ref_delta_test))
  expect_true("d18O_PDBCO2_wg" %in% colnames(ref_delta_test))
})
