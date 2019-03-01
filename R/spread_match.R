#' Spread and match sample gas and reference gas.
#'
# #' @param .data A [tibble][tibble::tibble-package], resulting from [correct_backgrounds()]
#' @inheritParams spread_intensities
#' @inheritParams match_intensities
#' @export
spread_match <- function(.data, method = "normal", quiet = default(quiet)) {
  .data %>%
    spread_intensities(quiet = quiet) %>%
    match_intensities(method = method, quiet = quiet)
}
