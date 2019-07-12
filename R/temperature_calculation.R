#' Calculate the temperatures
#'
#' This uses the [revcal()] function to calculate temperatures from
#' \eqn{\Delta_{47}}{Δ47} values. At the moment it ignores the uncertainty in
#' the regression.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from [acid_fractionation()].
#' @param D47 The column name of the \eqn{\Delta_47} values to use as input.
#' @param temp The column name of the output temperature.
#' @seealso revcal tempcal
#' @export
temperature_calculation <- function(.data, D47 = D47_final, temp = temperature,
                                    quiet = default(quiet)) {
  # global variables and defaults
  D47_final <- temperature <- NULL

  if (!quiet)
    message("Info: calculating temperature using default calibration, ignoring uncertainty in the calibration.")
  .data %>%
    mutate({{ temp }} := revcal({{ D47 }}, ignorecnf = TRUE))
}
