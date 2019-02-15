#' Add the information from the did files
#'
#' Append mass spectrometer metadata to the raw data.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from `collapse_cycles`
#' @param info A [tibble][tibble::tibble-package], resulting from `clean_did_info()`
#' @param ... Optional additional arguments to pass to the `left_join` call.
#' @export
#' @family metadata cleaning functions
add_info <- function(dat, info, ..., quiet = default(quiet)) {
  if (!quiet)
    message("Info: appending measurement information.")
  dat %>%
      left_join(info, ..., by = "file_id")
}
