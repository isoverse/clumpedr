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
#' # load libraries
#' library(ggplot2)
#' library(tibble)
#' # create an empty plot
#' dat <- tibble(age = 1:10, D47 = rnorm(10, 0.7, .1))
#' ggplot(dat, aes(x = age, y = D47)) +
#'   geom_point() +
#'   # generate the formula for temperature on the fly
#'   scale_y_continuous(sec.axis = sec_axis(temperature_axis()))
#' @seealso revcal
#' @seealso tempcal
#' @export
temperature_axis <- function(slope = 0.0449, intercept = 0.167) {
      ~ sqrt((slope * 1e6) / (. - intercept)) - 273.15
}

#' @name dots
#' @param ... These dots are for future extensions and must be empty.
NULL

#' @name quiet
#' @param quiet If `TRUE`, print info messages. If `NULL` (default) listens to `options(clumpedr.quiet)`
NULL
