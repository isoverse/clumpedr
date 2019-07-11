#' Clean up the did file info
#'
#' Converts mass spectrometer metadata into the correct types and appends the
#' initial intensity of mass 44.
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param ... Additional options to pass to [isoreader::iso_get_file_info()].
#' @inheritParams parse_info
#' @seealso [isoreader::iso_read_dual_inlet()]
#' @seealso [isoreader::iso_get_file_info()]
#' @family metadata cleaning functions
#' @export
clean_did_info  <- function(.did, ..., masspec = NULL, std_names = paste0("ETH-", 1:4),
                            oth_name = "other", quiet = default(quiet)) {
  if (!quiet) {
    message("Info: appending and parsing file info for {length(.did)} data file(s)")
  }

  parsed_info <- .did %>%
    parse_info(masspec, std_names, oth_name) %>%
    iso_get_file_info(.did, quiet = TRUE)

  inits <- get_inits(.did)

  outdf <- full_join(parsed_info, inits, by = "file_id")

  # convert back to list format
  file_info <-
    outdf %>%
    ensure_data_frame_list_columns() %>%
    split(seq(nrow(outdf)))

  # return
  map2(.did, file_info, ~{ .x$file_info <- .y; .x }) %>%
    iso_as_file_list()
}

#' Parse info into appropriate types.
#'
#' Converts the information from [isoreader::iso_get_file_info()] into the
#' appropriate types and appends new columns with masspec name and broadid.
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param masspec The name of the mass spectrometer to append.
#' @param std_names Character vector of the standard names to find in
#'     'Identifier 1'.
#' @param oth_name Single general name to assign to non-standard "other" measurements.
#' @param ms_name The name of the new masspec column.
#' @param broadid_name The name of the new broadid column.
#' @param id1 The column with sample/standard names.
#' @export
#' @family metadata cleaning functions
parse_info <- function(.did, masspec, std_names = paste0("ETH-", 1:4), oth_name = "other",
                       ms_name = masspec, broadid_name = broadid, id1 = `Identifier 1`) {
  # global variables and defaults
  broadid <- `Identifier 1` <- Row <- `Peak Center` <-
    Background <- Pressadjust <- `Reference Refill` <- Line <- Sample <-
      `Weight [mg]` <- Analysis <- Preparation <- NULL

  ms_name <- enquo(ms_name)
  broadid_name <- enquo(broadid_name)
  id1 <- enquo(id1)

  .did %>%
    iso_parse_file_info(double=c(Background, `Weight [mg]`),
                        integer=c(Row, Line, Sample, Analysis, Preparation),
                        logical=c(`Peak Center`, Pressadjust, `Reference Refill`), quiet=TRUE) %>%
    iso_mutate_file_info(
      # append new column infos
      !! ms_name := ifelse(!is.null(masspec), masspec, ""),
      !! broadid_name := ifelse(!! id1 %in% std_names, !! id1, oth_name),
      quiet=TRUE)
}

#' Get initial intensities of specified mass
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#'
#' @family metadata cleaning functions
#' @return A tibble with columns file_id, s44_init, and r44_init
#' @export
get_inits <- function(.did) {
  # global variables and defaults
  type <- cycle <- file_id <- value <- sample <- standard <- v44.mV <- NULL

  .did %>%
    isoreader::iso_get_raw_data(quiet = TRUE) %>%
    # filter first standard and first sample cycles
    filter(type == "standard" & cycle == 0 | type == "sample" & cycle == 1) %>%
    select(file_id, type, v44.mV) %>%
    spread(type, v44.mV) %>%
    select(file_id, s44_init = sample, r44_init = standard)
}

# import from isororeader for use in my parsing function
ensure_data_frame_list_columns <- utils::getFromNamespace("ensure_data_frame_list_columns", "isoreader")
