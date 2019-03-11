#' Plot disabled cycles
#'
#' @param .data A [tibble][tibble::tibble-package] with output from [disable_cycles()].
#' @param min Minimum intensity cutoff.
#' @param max Maximum intensity cutoff.
#' @family cycle functions
#' @export
plot_disabled_cycles  <- function(.data, min = 1500, max = 50000, quiet = default(quiet)) {
  # global variables and defaults
  grp <- file_id <- type <- cycle <- v44.mV <- grp <- cycle_dis <- type <- NULL

  .data  %>%
    mutate(grp = paste(file_id, type)) %>%
    ggplot(aes(x = cycle, y = v44.mV)) +
    geom_line(aes(colour = has_drop, group = grp), show.legend = FALSE) +
    geom_point(aes(colour = factor(cycle_dis), shape = factor(cycle_dis))) +
    ## scale_shape_manual(values = c(16, 16, 16, NA, 5)) +
    ## scale_alpha_manual(values = c(.4, .4, .4, .1, 1)) +
    ## scale_size_manual(values = c(3, 2, 2, 1, 3)) +
    ## scale_colour_manual(values = c("orange", "gray", "blue", "blue", "gray", "yellow", "black")) +

    geom_hline(yintercept = range(min, max), col = "indianred") +
    facet_grid(cols = vars(type))
}
