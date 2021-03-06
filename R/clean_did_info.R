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
    parse_info(masspec = masspec, std_names = std_names, oth_name = oth_name) %>%
    isoreader::iso_mutate_file_info(s44_init = inits$s44_init,
                                    r44_init = inits$r44_init)
}

#' Parse info into appropriate types.
#'
#' Converts the information from [isoreader::iso_get_file_info()] into the
#' appropriate types and appends new columns with masspec name and broadid.
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param masspec The name of the mass spectrometer to append.
#' @param std_names Character vector of the standard names to find in `Identifier 1`.
#' @param oth_name Single general name to assign to non-standard "other" measurements.
#' @param broadid_name The name of the new broadid column.
#' @param id1 The column with sample and standard names.
#' @export
#' @family metadata cleaning functions
parse_info <- function(.did, masspec = NA_character_, std_names = paste0("ETH-", 1:4), oth_name = "other",
                       broadid_name = broadid, id1 = `Identifier 1`) {
  # global variables and defaults
  broadid <- `Identifier 1` <- NULL

  .did %>%
    isoreader::iso_parse_file_info(double = c(.data$Background, .data$`Weight [mg]`),
                        integer = c(.data$Row, .data$Line, .data$Sample, .data$Analysis, .data$Preparation),
                        logical = c(.data$`Peak Center`, .data$Pressadjust, .data$`Reference Refill`), quiet = TRUE) %>%
    isoreader::iso_mutate_file_info(
      # append new column infos
      masspec = masspec,
      {{ broadid_name }} := ifelse({{ id1 }} %in% std_names, {{ id1 }}, oth_name),
      quiet = TRUE)
}

#' Get initial intensities of specified mass
#'
#' @param .did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#'
#' @family metadata cleaning functions
#' @return A tibble with columns file_id, s44_init, and r44_init
#' @export
get_inits <- function(.did) {
  .did %>%
    isoreader::iso_get_raw_data(quiet = TRUE) %>%
    # filter first standard and first sample cycles
    filter(.data$type == "standard" & .data$cycle == 0 | .data$type == "sample" & .data$cycle == 1) %>%
    pivot_wider(id_cols = .data$file_id, names_from = .data$type, values_from = .data$v44.mV) %>%
    select(.data$file_id, s44_init = .data$sample, r44_init = .data$standard) %>%
    tibble::as_tibble()
}

# import from isororeader for use in my parsing function
ensure_data_frame_list_columns <- utils::getFromNamespace("ensure_data_frame_list_columns", "isoreader")
