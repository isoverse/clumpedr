library(dplyr)
library(purrr)
library(isoreader)

test_that("parsing file info works", {
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Background"), "numeric")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Weight [mg]"), "numeric")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Row"), "integer")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Line"), "integer")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Sample"), "integer")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Analysis"), "integer")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Preparation"), "integer")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Peak Center"), "logical")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Pressadjust"), "logical")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("Reference Refill"), "logical")
})

test_that("appending file info works", {
  # does parse_info output an isofile?
  expect_is(parse_info(eth3), "iso_file")
  # do the new columns exist?
  expect_true("masspec" %in% (parse_info(eth3) %>%
                                iso_get_file_info() %>%
                                colnames()))
  expect_true("broadid" %in% (parse_info(eth3) %>%
                                iso_get_file_info() %>%
                                colnames()))
  # do the new columns consist of character vectors?
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("masspec"), "character")
  expect_is(parse_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("broadid"), "character")
})

test_that("getting inits works", {
  # it's a tibble
  expect_is(get_inits(eth3), "tbl_df")
  # it contains the desired columns
  expect_true("file_id" %in% colnames(get_inits(eth3)))
  expect_true("s44_init" %in% colnames(get_inits(eth3)))
  expect_true("r44_init" %in% colnames(get_inits(eth3)))
  # check that they are the right type
  expect_is(get_inits(eth3) %>% pluck("s44_init"), "numeric")
  expect_is(get_inits(eth3) %>% pluck("r44_init"), "numeric")
})

test_that("wrapper works", {
  # new columns are made
  expect_is(clean_did_info(eth3), "iso_file")
  expect_is(clean_did_info(standards), "iso_file_list")
  ## expect_true("masspec" %in% colnames(iso_get_file_info(clean_did_info(eth3))))
  expect_true("broadid" %in% (clean_did_info(eth3) %>%
                                iso_get_file_info() %>%
                                colnames()))
  expect_true("s44_init" %in% (clean_did_info(eth3) %>%
                                 iso_get_file_info() %>%
                                 colnames()))
  expect_true("r44_init" %in% (clean_did_info(eth3) %>%
                                 iso_get_file_info() %>%
                                 colnames()))
  ## # they are the right type
  expect_is(clean_did_info(eth3, "MOTU") %>%
              iso_get_file_info() %>%
              pluck("masspec"), "character")
  expect_is(clean_did_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("broadid"), "character")
  expect_is(clean_did_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("s44_init"), "numeric")
  expect_is(clean_did_info(eth3) %>%
              iso_get_file_info() %>%
              pluck("r44_init"), "numeric")
})
