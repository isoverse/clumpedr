#' Plot disabled cycles
#'
#' @param .data A [tibble][tibble::tibble-package] with output from [disable_cycles()].
#' @param min Minimum intensity cutoff.
#' @param max Maximum intensity cutoff.
#' @family cycle functions
#' @export
plot_disabled_cycles  <- function(.data, y = v44.mV, min = 1500, max = 50000, quiet = default(quiet)) {
  # global variables and defaults
  grp <- file_id <- type <- cycle <- v44.mV <- expected_D47 <- grp <- cycle_dis <- has_drop <-
    `Identifier 1` <- Preparation <- type <- NULL

  y <- enquo(y)

  .data  %>%
    mutate(grp = paste(file_id, type),
           has_drop = ifelse(has_drop, "ali_drop", "ali_nodrop")) %>%
    unite("cycle_meta", cycle_dis, has_drop, sep = " ") %>%
    ggplot(aes(x = cycle, y = !! y, colour = factor(cycle_meta), shape = factor(cycle_meta),
           alpha = factor(cycle_meta))) +
    geom_line(aes(group = grp), show.legend = FALSE) +
    geom_point() +
    scale_shape_manual(values = c(16, 16, 16, NA, 5)) +
    scale_alpha_manual(values = c(1, 1, 1, .3, 1)) +
    scale_size_manual(values = c(1, 4, 2, 1, 5)) +
    scale_colour_manual(values = c("orange", "red", "darkgreen", "gray", "red")) +
    geom_hline(yintercept = range(min, max), col = "indianred", linetype = 2) +
    facet_grid(cols = vars(type))
}
