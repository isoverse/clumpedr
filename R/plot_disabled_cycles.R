#' Plot disabled cycles
#'
#' @param .data A [tibble][tibble::tibble-package] with output from [disable_cycles()].
#' @param y Variable to put on the y-axis.
#' @param min Minimum intensity cutoff.
#' @param max Maximum intensity cutoff.
#' @family cycle functions
#' @export
plot_disabled_cycles  <- function(.data, y = v44.mV, min = 1500, max = 50000, quiet = default(quiet)) {
  # global variables and defaults
  v44.mV <- NULL

  .data  %>%
    mutate(cycle_meta = case_when(.data$v44_low ~ "v44_low",
                                  .data$v44_high ~ "v44_high",
                                  .data$v44_drop ~ "v44_drop",
                                  .data$drop_before ~ "drop_before",
                                  .data$has_drop ~ "has_drop",
                                  TRUE ~ "no_drop",
                                  ) %>%
             factor(levels = c("v44_low", "v44_high", "v44_drop", "drop_before", "has_drop", "no_drop"))) %>%
    ggplot(aes(x = .data$cycle, y = {{ y }}, colour = .data$cycle_meta, shape = .data$cycle_meta, alpha = .data$cycle_meta, size = .data$cycle_meta)) +
    geom_line(aes(group = .data$file_id), alpha = .5) +
    geom_point() +
    scale_shape_manual(values = c(16, 16, 15, 16, 16, NA),
                       drop = FALSE) +
    scale_alpha_manual(values = c(1, 1, 1, 1, 1, .1),
                       drop = FALSE) +
    scale_size_manual(values = c(2, 2, 5, 2, 1, .5),
                      drop = FALSE) +
    scale_colour_manual(values = c("steelblue", "indianred", "red", "orange", "darkgreen", "gray"),
                        drop = FALSE) +
    geom_hline(yintercept = range(min, max), col = "indianred", linetype = 2) +
    facet_grid(cols = vars(.data$type))
}
