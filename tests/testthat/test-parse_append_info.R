context("Parsing and Appending File Information")

test_that("parse_info works", {
  # does parse_info output an isofile?
  expect_is(parse_info(eth3), "iso_file")
})

test_that("get_inits works", {
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

test_that("clean_did_info works", {
  expect_is(clean_did_info(eth3), "iso_file")
  expect_is(clean_did_info(standards), "iso_file_list")
  inf <- clean_did_info(eth3, "MOTU") %>% iso_get_file_info()

  expect_is(inf %>% pluck("Background"), "numeric")
  expect_is(inf %>% pluck("Weight [mg]"), "numeric")
  expect_is(inf %>% pluck("Row"), "integer")
  expect_is(inf %>% pluck("Line"), "integer")
  expect_is(inf %>% pluck("Sample"), "integer")
  expect_is(inf %>% pluck("Analysis"), "integer")
  expect_is(inf %>% pluck("Preparation"), "integer")
  expect_is(inf %>% pluck("Peak Center"), "logical")
  expect_is(inf %>% pluck("Pressadjust"), "logical")
  expect_is(inf %>% pluck("Reference Refill"), "logical")

  cln <- clean_did_info(eth3, "MOTU") %>% iso_get_file_info() %>% colnames()
  expect_true("masspec" %in% cln)
  expect_true("broadid" %in% cln)
  expect_true("s44_init" %in% cln)
  expect_true("r44_init" %in% cln)
  ## # they are the right type
# do the new columns consist of character vectors?
  expect_is(inf %>% pluck("masspec"), "character")
  expect_is(inf %>% pluck("broadid"), "character")
  expect_is(inf %>% pluck("s44_init"), "numeric")
  expect_is(inf %>% pluck("r44_init"), "numeric")
})


test_that("adding file information works", {
  expect_is(standards %>%
    isoreader::iso_get_raw_data() %>%
    find_bad_cycles() %>%
    spread_match() %>%
    append_ref_deltas(standards) %>%
    delta_values(genplot=FALSE) %>%
      collapse_cycles() %>%
      add_info(isoreader::iso_get_file_info(standards)), "tbl_df")
})
