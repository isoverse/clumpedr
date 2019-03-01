#' Plot as a side-effect of a pipe
#'
#' This function takes a tibble, applies a plotting function with additional
#' arguments, makes sure to print it, and then returns the original
#' [tibble][tibble::tibble-package] unaffected.
#'
#' @details This is equivalent to running the [magrittr::%T>%] operator with
#'   curly braces and a print command, but this is much cleaner.
#'
#' @param .data Tibble in the pipe.
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
