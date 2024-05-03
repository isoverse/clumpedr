#' Calculate little deltas from abundance ratios
#'
#' For all intensities \eqn{i},
#' \deqn{\delta_i = (R_i / R_iwg - 1) \times 1000}
#'
#' @param .data A [tibble][tibble::tibble-package] with abundance ratios.
#' @export
little_deltas <- function(.data, ..., quiet = NULL) {
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  if (!quiet) {
    message("Info: calculating \u03b4 values with (Ri / Ri_wg - 1) * 1000")
  }

  .data %>%
    mutate(
      d45 = (.data$R45 / .data$R45_wg - 1) * 1000,
      d46 = (.data$R46 / .data$R46_wg - 1) * 1000,
      d47 = (.data$R47 / .data$R47_wg - 1) * 1000,
      d48 = (.data$R48 / .data$R48_wg - 1) * 1000,
      d49 = (.data$R49 / .data$R49_wg - 1) * 1000)
}
