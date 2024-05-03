#' Calculate the temperatures
#'
#' This uses the [revcal()] function to calculate temperatures from
#' \eqn{\Delta_{47}} values. At the moment it ignores the uncertainty in
#' the regression.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from [acid_fractionation()].
#' @param D47 The column name of the \eqn{\Delta_47} values to use as input.
#' @param temp The column name of the output temperature.
#' @param slope Character(1) column name with the slope.
#' @param intercept Character(1) column name with the intercept.
#' @seealso revcal tempcal
#' @export
temperature_calculation <- function(.data, ...,
                                    D47 = D47_final, temp = temperature,
                                    slope = "slope", intercept = "intercept",
                                    quiet = default(quiet)) {
  # global variables and defaults
  D47_final <- temperature <- NULL

  if (!quiet) {
    message(glue::glue("Info: calculating temperature with slope {unique(.data[, slope])} and intercept {unique(.data[, intercept])}, ignoring uncertainty in the calibration."))
    message("If you would like to include temperature uncertainty using bootstrapping, see the package `clumpedcalib` on <https://github.com/japhir/clumpedcalib>")
  }

  .data %>%
    mutate({{ temp }} := revcal({{ D47 }}, slope = .data[[slope]], intercept = .data[[intercept]], ignorecnf = TRUE))
}
