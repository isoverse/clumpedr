#' Plot as a side-effect of a pipe
#'
#' This function takes a tibble, applies a plotting function with additional
#' arguments, makes sure to print it, and then returns the original
#' [tibble][tibble::tibble-package] unaffected.
#'
#' @details This is equivalent to running the [magrittr::%T>%] operator with
#'   curly braces and a print command, but this is much cleaner.
#'
#' @param .data The data to use for plotting.
#' @param plotfun Function to use for plotting.
#' @param ... Additional arguments to the plotting function.
#'
# example commented out because it fails the build
# #' @examples
# #' # create an example tibble
# #' .data <- tibble::tibble(x = 1:10, y = 11:20)
# #' # an example of a plotting function that would normally not print in a pipe.
# #' pointplot <- function(.data, ...) {
# #'   ggplot2::ggplot(.data, aes(x = x, y = y)) +
# #'   ggplot2::geom_point(...)
# #' }
# #' .data %>%
# #'   pointplot()
# #'   # .dataa not returned
# #' # calling the function within the pipe now prints it and returns the input!
# #' .data %>%
# #'   pipe_plot(pointplot) %>%
# #'   glimpse()
#' @export
pipe_plot <- function(.data, plotfun, ...) {
    .data %>%
        plotfun(...) %>%
        print()
    .data
}

#' Generate a formula for a secondary temperature y-axis
#'
#' Uses the clumped isotope temperature calibration of the form:
#' \deqn{y = a \times 10^6 / T^2 + b}{y = a * 10^6 / T^2 + b}
#'
#' Defaults to Bonifacie et al. 2017
#' \deqn{\Delta_47 = (0.0449 \pm 0.001 \times 10^6) / T^2 + (0.167 \pm 0.01)}{Δ47 = (0.0499 ± 0.001 * 10^6) / T^2 + (0.167 ± 0.01)}
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
