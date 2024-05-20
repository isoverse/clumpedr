#' Generate a formula for a secondary temperature y-axis
#'
#' Uses the clumped isotope temperature calibration of the form:
#' \deqn{y = a \times 10^6 / T^2 + b}{y = a * 10^6 / T^2 + b}
#'
#' Defaults to Bonifacie et al. 2017
#' \deqn{\Delta_47 = (0.0449 \pm 0.001 \times 10^6) / T^2 + (0.167 \pm 0.01)}
#'
#' @param slope The slope of the calibration.
#' @param intercept The intercept of the calibration.
#' @examples
#' dat <- tibble::tibble(age = 1:10, D47 = rnorm(10, 0.7, .1))
#' # create an empty plot
#' ggplot2::ggplot(dat, ggplot2::aes(x = age, y = D47)) +
#'   ggplot2::geom_point() +
#'   # generate the formula for temperature on the fly
#'   ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(temperature_axis()))
#' @seealso temperature_calculation()
#' @seealso revcal()
#' @seealso tempcal()
#' @export
temperature_axis <- function(slope = 0.0449, intercept = 0.167) {
      ~ sqrt((slope * 1e6) / (. - intercept)) - 273.15
}

#' @name quiet
#' @param quiet If `TRUE`, print info messages. If `NULL` (default) listens to `options(clumpedr.quiet)`
NULL

check_quiet <- function(quiet = NULL) {
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }
  if (!is.logical(quiet)) {
    stop("'quiet' must be logical (TRUE or FALSE).", call. = FALSE)
  }
  if (is.na(quiet)) {
    stop("'quiet' must be TRUE/FALSE, not NA.", call. = FALSE)
  }
  return(quiet)
}
