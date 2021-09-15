context("Appending Reference gas \u03b4 values")

test_that("get_ref_delta works", {
  expect_is(get_ref_delta(standards), "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(get_ref_delta(standards)))
  expect_true("d18O_PDBCO2_wg" %in% colnames(get_ref_delta(standards)))
})

test_that("append_ref_deltas works", {
  expect_error(append_ref_deltas(), "argument \".data\" is missing, with no default",
               fixed = TRUE)
  expect_error(append_ref_deltas(.data = "hoi"),
               ".data must be a data.frame or tibble", fixed = TRUE)
  expect_error(append_ref_deltas(.data = tibble(a = 1:10, b = 11:20)),
               "Either .did or d13C_PDB_wg and d18O_PDBCO2_wg must be provided.",
               fixed = TRUE)
  expect_error(append_ref_deltas(.data = tibble(a = 1:10, b = 11:20), .did = "hoi"),
               ".did must be an 'iso_file' or 'iso_file_list'.", fixed = TRUE)
  expect_error(append_ref_deltas(.data = tibble(a = 1:10, b = 11:20), .did = standards),
               ".data must contain the column 'file_id'.", fixed = TRUE)
  ref_delta_test <- standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards)
  expect_is(ref_delta_test, "tbl_df")
  expect_true("d13C_PDB_wg" %in% colnames(ref_delta_test))
  expect_true("d18O_PDBCO2_wg" %in% colnames(ref_delta_test))
})
