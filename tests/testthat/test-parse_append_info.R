test_that("parse_info works", {
  # does parse_info output an isofile?
  expect_s3_class(parse_info(eth3), "iso_file")
})

test_that("get_inits works", {
  # it's a tibble
  expect_s3_class(get_inits(isoreader::iso_get_raw_data(eth3, include_file_info = "Analysis")), "tbl_df")
  # it contains the desired columns
  ini <- get_inits(isoreader::iso_get_raw_data(eth3, include_file_info = "Analysis"))
  expect_true("file_id" %in% colnames(ini))
  expect_true("s44_init" %in% colnames(ini))
  expect_true("r44_init" %in% colnames(ini))
  # check that they are the right type
  expect_type(ini %>% pluck("s44_init"), "double")
  expect_type(ini %>% pluck("r44_init"), "double")
})

test_that("clean_did_info works", {
  expect_s3_class(clean_did_info(eth3), "iso_file")
  expect_s3_class(clean_did_info(standards), "iso_file_list")
  inf <- clean_did_info(eth3, "MOTU") %>% isoreader::iso_get_file_info()

  expect_type(inf %>% pluck("Background"), "double")
  expect_type(inf %>% pluck("Weight [mg]"), "double")
  expect_type(inf %>% pluck("Row"), "integer")
  expect_type(inf %>% pluck("Line"), "integer")
  expect_type(inf %>% pluck("Sample"), "integer")
  expect_type(inf %>% pluck("Analysis"), "integer")
  expect_type(inf %>% pluck("Preparation"), "integer")
  expect_type(inf %>% pluck("Peak Center"), "logical")
  expect_type(inf %>% pluck("Pressadjust"), "logical")
  expect_type(inf %>% pluck("Reference Refill"), "logical")

  cln <- clean_did_info(eth3, "MOTU") %>%
    isoreader::iso_get_file_info() %>% colnames()
  expect_true("masspec" %in% cln)
  expect_true("broadid" %in% cln)
  expect_true("s44_init" %in% cln)
  expect_true("r44_init" %in% cln)
  ## # they are the right type
# do the new columns consist of character vectors?
  expect_type(inf %>% pluck("masspec"), "character")
  expect_type(inf %>% pluck("broadid"), "character")
  expect_type(inf %>% pluck("s44_init"), "double")
  expect_type(inf %>% pluck("r44_init"), "double")
})


## test_that("adding file information works", {
##   expect_is(standards %>%
##     isoreader::iso_get_raw_data() %>%
##     find_bad_cycles() %>%
##     spread_match() %>%
##     append_ref_deltas(standards) %>%
##     delta_values() %>%
##     ## collapse_cycles() %>%
##     add_info(isoreader::iso_get_file_info(standards)), "tbl_df")
## })
