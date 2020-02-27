#' Spread and match sample gas and reference gas.
#'
# #' @param .data A [tibble][tibble::tibble-package], resulting from [correct_backgrounds()]
#' @inheritParams match_intensities
#' @export
spread_match <- function(.data, method = "normal", masses = c(44:49, 54), quiet = default(quiet)) {
  .data %>%
    spread_intensities(our_cols = paste0("v", masses, ".mV"), quiet = quiet) %>%
    match_intensities(method = method, masses = masses, quiet = quiet)
}
