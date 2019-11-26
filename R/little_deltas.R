#' Calculate little deltas from abundance ratios
#'
#' For all intensities \eqn{i},
#' \deqn{\delta_i = (R_i / R_iwg - 1) \times 1000}
#'
#' @param .data A [tibble][tibble::tibble-package] with abundance ratios.
little_deltas <- function(.data, quiet = default(quiet)) {
  # global variables and defaults
  d45 <- d46 <- d47 <- d48 <- d48 <- d49 <- R45 <- R46 <- R47 <- R48 <- R48 <-
    R49 <- R45_wg <- R46_wg <- R47_wg <- R48_wg <- R48_wg <- R49_wg <- NULL

  if (!quiet)
    message("Info: calculating \u03b4 values with (Ri / Ri_wg - 1) * 1000")
  .data %>%
    mutate(
      d45 = (R45 / R45_wg - 1) * 1000,
      d46 = (R46 / R46_wg - 1) * 1000,
      d47 = (R47 / R47_wg - 1) * 1000,
      d48 = (R48 / R48_wg - 1) * 1000,
      d49 = (R49 / R49_wg - 1) * 1000)
}
