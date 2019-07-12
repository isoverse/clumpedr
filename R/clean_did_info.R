#' Clean up the did file info
#'
#' Converts mass spectrometer metadata into the correct types and appends the
#' initial intensity of mass 44.
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @inheritParams parse_info
#' @seealso [isoreader::iso_read_dual_inlet()]
#' @seealso [isoreader::iso_get_file_info()]
#' @family metadata cleaning functions
#' @export
clean_did_info  <- function(.did, masspec = NULL, std_names = paste0("ETH-", 1:4),
                            oth_name = "other", quiet = default(quiet)) {
  if (!quiet) {
    message(glue("Info: appending and parsing file info for {length(.did)} data file(s)"))
  }

  inits <- get_inits(.did)

  .did %>%
    parse_info(masspec=masspec, std_names=std_names, oth_name=oth_name) %>%
    isoreader::iso_mutate_file_info(s44_init=inits$s44_init,
                                    r44_init=inits$r44_init)
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
parse_info <- function(.did, masspec=NA_character_, std_names = paste0("ETH-", 1:4), oth_name = "other",
                       ms_name = masspec, broadid_name = broadid, id1 = `Identifier 1`) {
  # global variables and defaults
  broadid <- `Identifier 1` <- Row <- `Peak Center` <-
    Background <- Pressadjust <- `Reference Refill` <- Line <- Sample <-
      `Weight [mg]` <- Analysis <- Preparation <- NULL

  ms_name <- enquo(ms_name)
  broadid_name <- enquo(broadid_name)
  id1 <- enquo(id1)

  .did %>%
    isoreader::iso_parse_file_info(double=c(Background, `Weight [mg]`),
                        integer=c(Row, Line, Sample, Analysis, Preparation),
                        logical=c(`Peak Center`, Pressadjust, `Reference Refill`), quiet=TRUE) %>%
    isoreader::iso_mutate_file_info(
      # append new column infos
      !! ms_name := masspec,
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
    pivot_wider(id_cols=file_id, names_from=type, values_from=v44.mV) %>%
    select(file_id, s44_init = sample, r44_init = standard) %>%
    tibble::as_tibble()
}

# import from isororeader for use in my parsing function
ensure_data_frame_list_columns <- utils::getFromNamespace("ensure_data_frame_list_columns", "isoreader")
