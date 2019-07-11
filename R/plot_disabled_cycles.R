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
  grp <- file_id <- type <- cycle <- v44.mV <- expected_D47 <- grp <- cycle_dis <- has_drop <-
    `Identifier 1` <- Preparation <- type <- cycle_meta <- NULL

  y <- enquo(y)

  .data  %>%
    mutate(cycle_meta = case_when(v44_low ~ "v44_low",
                                  v44_high ~ "v44_high",
                                  v44_drop ~ "v44_drop",
                                  drop_before ~ "drop_before",
                                  has_drop ~ "has_drop",
                                  TRUE ~ "no_drop",
                                  ) %>%
             factor(levels=c("v44_low", "v44_high", "v44_drop", "drop_before", "has_drop", "no_drop"))) %>%
    ggplot(aes(x=cycle, y=!! y, colour=cycle_meta, shape=cycle_meta, alpha=cycle_meta, size=cycle_meta)) +
    geom_line(aes(group=file_id), alpha = .5) +
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
    facet_grid(cols = vars(type))
}
