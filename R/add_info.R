#' Add the information from the did files
#'
#' Append mass spectrometer metadata to the raw data.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [collapse_cycles()].
#' @param .info A [tibble][tibble::tibble-package], resulting from
#'   [clean_did_info()].
#' @param cols  A character vector with column names in info to add to the data.
#' @param ... Optional additional arguments to pass to the [dplyr::left_join()]
#'   call.
#' @export
#' @family metadata cleaning functions
add_info <- function(.data, .info, cols, quiet = default(quiet)) {
  if (nrow(.data) == 0) {
    return(tibble(file_id = character()))
  }

  if (!"file_id" %in% cols) {
    cols <- c("file_id", cols)
  }

  if (!quiet)
    message("Info: appending measurement information.")
    left_join(x = .data, y = .info %>% select(tidyselect::all_of(cols)), by = "file_id")
}
