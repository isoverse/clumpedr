#' Add the information from the did files
#'
#' Append mass spectrometer metadata to the raw data.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [collapse_cycles()].
#' @param .info A [tibble][tibble::tibble-package], resulting from
#'   [clean_did_info()].
#' @param cols  A character vector with column names in info to add to the data.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with new columns in `cols`.
#' @examples
#' # generate some artificial data
#' dat <- tibble(file_id = c("hi", "bye"), Analysis = c(1, 2))
#' # and artificial metadata
#' inf <- tibble(file_id = c("hi", "bye"), Analysis = c(1, 2),
#'               file_path = c("/path/to/file", "/path/to/file"),
#'               weight = c(69, 87), )
#' dat <- dat |>
#'   add_info(inf, c("weight", "file_path"))
#' @export
#' @family metadata cleaning functions
add_info <- function(.data, .info, ...,
                     cols = NULL, quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0) {
    return(tibble(file_id = character()))
  }
  quiet <- check_quiet(quiet)

  if (is.null(cols)) {
    cols <- c("file_id", "Analysis")
  }

  if (!"file_id" %in% cols) {
    cols <- c("file_id", cols)
  }

  if (!"Analysis" %in% cols) {
    cols <- c("Analysis", cols)
  }

  if (!quiet) {
    message("Info: appending measurement information.")
  }
  left_join(x = .data, y = .info %>% select(tidyselect::all_of(cols)), by = c("Analysis", "file_id"))
}
