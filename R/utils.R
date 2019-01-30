#' Plot as a side-effect of a pipe
#'
#' This function takes a tibble, applies a plotting function with additional
#' arguments, makes sure to print it, and then returns the original dataframe
#' unaffected.
#'
#' @details This is equivalent to running the `magrittr::%T>%` operator with
#'     curly braces and a print command, but this is much cleaner.
#'
#' @param dat Tibble in the pipe.
#' @param plotfun Function to use for plotting.
#' @param ... Additional arguments to the plotting function.
#'
#' @examples
#' # create an example tibble
#' dat <- tibble(x = 1:10, y = 11:20)
#' # an example of a plotting function that would normally not print in a pipe.
#' pointplot <- function(dat, ...) {
#'   ggplot(dat, aes(x = x, y = y)) +
#'   geom_point(...)
#' }
#' # calling the function within the pipe now prints it and returns the input!
#' dat %>%
#'   pipe_plot(pointplot) %>%
#'   glimpse()
#' @export
pipe_plot <- function(dat, plotfun, ...) {
    dat %>%
        plotfun(...) %>%
        print()
    dat
}
