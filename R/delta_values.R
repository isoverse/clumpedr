#' Calculate abundance ratios, \eqn{\delta^{18}}O, \eqn{\delta^{13}}C, \eqn{\delta}'s, and raw \eqn{\Delta} values
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [correct_backgrounds()].
#' @details This function is a wrapper for the [abundance_ratios()],
#'   [little_deltas()], and [bulk_and_clumping_deltas()] functions.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
delta_values <- function(.data, ..., quiet = NULL) {
  R45_wg <- R46_wg <- R47_wg <- R48_wg <- R49_wg <- NULL
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  .data %>%
    # sample gas
    abundance_ratios(quiet = quiet) %>%
    # working gas
    abundance_ratios(i44 = .data$r44, i45 = .data$r45, i46 = .data$r46, i47 = .data$r47, i48 = .data$r48,
                     i49 = .data$r49, R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                     R48 = R48_wg, R49 = R49_wg, quiet = TRUE) %>%
    little_deltas(quiet = quiet) %>%
    bulk_and_clumping_deltas(quiet = quiet)
}
