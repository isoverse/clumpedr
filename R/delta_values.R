#' Calculate abundance ratios, \eqn{\delta^{18}}{δ18}O, \eqn{\delta^{13}}{δ13}C, \eqn{\delta}{δ}'s, and raw \eqn{\Delta}{Δ} values
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [correct_backgrounds()].
#' @details This function is a wrapper for the [abundance_ratios()],
#'   [little_deltas()], and [bulk_and_clumping_deltas()] functions.
#'
#' @export
delta_values <- function(.data, quiet = default(quiet)) {
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  .data %>%
    # sample gas
    abundance_ratios(quiet = quiet) %>%
    # working gas
    abundance_ratios(i44 = .data$r44, i45 = .data$r45, i46 = .data$r46, i47 = .data$r47, i48 = .data$r48,
                     i49 = .data$r49, R45 = .data$R45_wg, R46 = .data$R46_wg, R47 = .data$R47_wg,
                     R48 = .data$R48_wg, R49 = .data$R49_wg, quiet = TRUE) %>%
    little_deltas(quiet = quiet) %>%
    bulk_and_clumping_deltas(quiet = quiet)
}
