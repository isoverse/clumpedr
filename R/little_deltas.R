#' Calculate little deltas from abundance ratios
#'
#' For all intensities \eqn{i},
#' \deqn{\delta_i = (R_i / R_iwg - 1) \times 1000}
#'
#' @param .data A [tibble][tibble::tibble-package] with abundance ratios.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with new columns d45-d49.
#' @examples
#' # generate some artificial data to show how it works
#' x <- function() rnorm(n = 10)
#' dat <- tibble::tibble(R45 = x(), R46 = x(), R47 = x(), R48 = x(), R49 = x(),
#'                       R45_wg = x(), R46_wg = x(), R47_wg = x(), R48_wg = x(), R49_wg = x())
#' dat <- dat |>
#'    little_deltas()
#' @export
little_deltas <- function(.data, ..., quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  quiet <- check_quiet(quiet)

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
