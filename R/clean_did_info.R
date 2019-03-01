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
    message("Info: appending and parsing file info")
    # TODO: include parameters  with std_names = {stringr::str_c(std_names)} and oth_name = {oth_name}
  }
  parsedinfo <- .did %>%
    iso_get_file_info(...) %>%
    parse_info(masspec, std_names, oth_name)

  inits <- get_inits(.did)

  full_join(parsedinfo, inits, by = "file_id")
}

#' Parse info into appropriate types.
#'
#' Converts the information from [isoreader::iso_get_file_info()] into the
#' appropriate types.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [isoreader::iso_get_file_info()].
#' @param masspec The name of the mass spectrometer to append.
#' @param std_names Character vector of the standard names to find in
#'     'Identifier 1'.
#' @param oth_name Single general name to assign to non-standard "other" measurements.
#' @param ms_name The name of the new masspec column.
#' @param broadid_name The name of the new broadid column.
#' @param id1 The column with sample/standard names.
#' @export
#' @family metadata cleaning functions
parse_info <- function(.data, masspec, std_names = paste0("ETH-", 1:4), oth_name = "other",
                       ms_name = masspec, broadid_name = broadid, id1 = `Identifier 1`) {
  ms_name <- enquo(ms_name)
  broadid_name <- enquo(broadi_name)
  id1 <- enquo(id1)

  .data %>%
    mutate(
      Row = parse_integer(Row),
      `Peak Center` = parse_logical(`Peak Center`),
      Background = parse_double(Background),
      Pressadjust = parse_logical(Pressadjust),
      `Reference Refill` = parse_logical(`Reference Refill`),
      Line = parse_integer(Line),
      Sample = parse_integer(Sample),
      `Weight [mg]` = parse_double(`Weight [mg]`),
      Analysis = parse_integer(Analysis),
      Preparation = parse_integer(Preparation),
      # append new column infos
      !! ms_name := ifelse(!is.null(masspec), masspec, ""),
      !! broadid_name := ifelse(!! id1 %in% std_names, !! id1, oth_name))
}

#' Add initial intensities
#'
#' @param .did the did data resulting from [isoreader::iso_read_dual_inlet()].
#'
#' @family metadata cleaning functions
#' @export
get_inits <- function(.did) {
  # TODO: document this function better.

  # TODO: check if all the columns are present, otherwise stop?
  .did %>%
    isoreader::iso_get_raw_data(quiet = TRUE) %>%
    # filter first standard and first sample cycles
    filter(type == "standard" & cycle == 0 | type == "sample" & cycle == 1) %>%
    # TODO: check if v44.mV is present
    select(file_id, type, v44.mV) %>%
    spread(type, v44.mV) %>%
    select(file_id, s44_init = sample, r44_init = standard)
}
