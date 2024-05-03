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
clean_did_info  <- function(.did, masspec = NULL,
                            std_names = c(paste0("ETH-", 1:4), "IAEA-C2", "Merck"),
                            oth_name = "other",
                            quiet = NULL) {
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  if (!quiet) {
    message(glue("Info: appending and parsing file info for {length(.did)} data file(s)"))
  }

  if (!requireNamespace("isoreader", quietly = TRUE)) {
    stop("'isoreader' is required to clean_did_info, please run:\n   remotes::install_github('isoverse/isoreader')",
         call. = FALSE)
    return(invisible(.did))
  }

  inits <- get_inits(isoreader::iso_get_raw_data(.did, include_file_info = "Analysis"))

  .did %>%
    parse_info(masspec = masspec, std_names = std_names, oth_name = oth_name) %>%
    isoreader::iso_add_file_info(inits, "file_id")
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
parse_info <- function(.did,
                       masspec = NA_character_,
                       std_names = paste0("ETH-", 1:4),
                       oth_name = "other",
                       broadid_name = broadid,
                       id1 = `Identifier 1`) {
  # global variables and defaults
  broadid <- `Identifier 1` <- NULL

  if (!requireNamespace("isoreader", quietly = TRUE)) {
    stop("'isoreader' is required to parse_info, please run:\n   remotes::install_github('isoverse/isoreader')",
         call. = FALSE)
    return(invisible(.did))
  }

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
#' @param .did The raw data, resulting from [isoreader::iso_get_raw_data()].
#'
#' @family metadata cleaning functions
#' @return A tibble with columns file_id, s44_init, and r44_init
#' @export
get_inits <- function(.did) {
  # what to do if did is an empty dataframe?
  if (is.data.frame(.did) && nrow(.did) == 0L) {
    return(
      tibble(file_id = character())#, #Analysis = character(),
             ## s44_init = double(), r44_init = double())
    )
  }

  .did %>%
    # filter first standard and first sample cycles
    filter(.data$type == "standard" & .data$cycle == 0 | .data$type == "sample" & .data$cycle == 1) %>%
    pivot_wider(id_cols = c("file_id", "Analysis"), names_from = "type", values_from = "v44.mV") %>%
    select("file_id", "Analysis", s44_init = "sample", r44_init = "standard") %>%
    tibble::as_tibble()
}
