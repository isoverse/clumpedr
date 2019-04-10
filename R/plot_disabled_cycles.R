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
    mutate(has_drop = ifelse(has_drop, "ali_drop", "ali_nodrop")) %>%
    unite("cycle_meta", cycle_dis, has_drop, sep = " ") %>%
    mutate(cycle_meta = factor(cycle_meta,
                               levels = c("no_drop ali_nodrop",
                                          "low_v44 NA",
                                          "high_v44 NA",
                                          "no_drop ali_drop",
                                          "drop_before ali_drop",
                                          "pressure_drop ali_drop"))) %>%
    ggplot(aes(x = cycle, y = !! y, colour = cycle_meta, shape = cycle_meta, alpha = cycle_meta)) +
    geom_line(aes(group = file_id), alpha = .5) +
    geom_point() +
    scale_shape_manual(values = c(NA, 16, 16, 16, 16, 5)) +
    scale_alpha_manual(values = c(.2, 1, 1, 1, 1, 1)) +
    scale_size_manual(values = c(1, 1, 2, 3, 4, 5)) +
    scale_colour_manual(values = c("gray", "steelblue", "indianred", "darkgreen", "orange", "red")) +
    geom_hline(yintercept = range(min, max), col = "indianred", linetype = 2) +
    facet_grid(cols = vars(type))
}
