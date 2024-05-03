test_that("abundance_ratios() works", {
  expect_equal(abundance_ratios(.data = tibble()), tibble(file_id = character()))
  x <- function() rnorm(n = 10)
  abundance_test <- tibble(s44 = x(), s45 = x(), s46 = x(), s47 = x(), s48 = x(), s49 = x()) |>
    abundance_ratios()
  expect_s3_class(abundance_test, "tbl_df")
  expect_true("R45" %in% colnames(abundance_test))
  expect_true("R46" %in% colnames(abundance_test))
  expect_true("R47" %in% colnames(abundance_test))
  expect_true("R48" %in% colnames(abundance_test))
  expect_true("R49" %in% colnames(abundance_test))
})

test_that("little_deltas() works", {
  expect_equal(little_deltas(.data = tibble()), tibble(file_id = character()))
  x <- function() rnorm(n = 10)
  deltas_test <- tibble(R45 = x(), R46 = x(), R47 = x(), R48 = x(), R49 = x(),
                        R45_wg = x(), R46_wg = x(), R47_wg = x(), R48_wg = x(), R49_wg = x()) |>
    little_deltas()
  expect_s3_class(deltas_test, "tbl_df")
  expect_true("d45" %in% colnames(deltas_test))
  expect_true("d46" %in% colnames(deltas_test))
  expect_true("d47" %in% colnames(deltas_test))
  expect_true("d48" %in% colnames(deltas_test))
  expect_true("d49" %in% colnames(deltas_test))
})

test_that("bulk_and_clumping_deltas() works", {
  expect_equal(bulk_and_clumping_deltas(.data = tibble()), tibble(file_id = character()))
  x <- function() rnorm(n = 10)
  bulk_clump_test <- tibble(
    s44 = x(), s45 = x(), s46 = x(), s47 = x(), s48 = x(), s49 = x(),
    r44 = x(), r45 = x(), r46 = x(), r47 = x(), r48 = x(), r49 = x(),
    d45 = x(), d46 = x(), d47 = x(), d48 = x(), d49 = x(),
    d13C_PDB_wg = x(), d18O_PDBCO2_wg = x()) |>
    bulk_and_clumping_deltas()

  expect_s3_class(bulk_clump_test, "tbl_df")
  expect_true("D47_raw" %in% colnames(bulk_clump_test))
  expect_true("D48_raw" %in% colnames(bulk_clump_test))
  expect_true("D49_raw" %in% colnames(bulk_clump_test))
  expect_true("param_49" %in% colnames(bulk_clump_test))
})

test_that("delta_values() wrapper works", {
  # let's test this with the real deal?
  expect_s3_class(test_wrapper <- standards %>%
    isoreader::iso_get_raw_data(include_file_info = "Analysis") %>%
    mutate(min = 1500, max = 50000, fac = 3) |>
    find_bad_cycles(min = "min", max = "max", fac = "fac") %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(), "tbl_df")
  expect_snapshot(test_wrapper)
})
