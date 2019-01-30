#' add_info
#'
#' Add the info from the did files.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from `collapse_cycles`
#' @param info A [tibble][tibble::tibble-package], resulting from `clean_did_info()`
add_info <- function(dat, info, ..., quiet = default(quiet)) {
  if (!quiet)
    message("Info: appending measurement information.")
  dat %>%
      left_join(info, ..., by = "file_id")
}
