#' Plot disabled cycles
#'
#' @param .data A [tibble][tibble::tibble-package] with output from [disable_cycles()].
#' @param min Minimum intensity cutoff.
#' @param max Maximum intensity cutoff.
#' @family cycle functions
#' @export
plot_disabled_cycles  <- function(.data, min = 1500, max = 50000, quiet = default(quiet)) {
  # global variables and defaults
  grp <- linegrps <- file_id <- hasdrop <- cycle_dis <- cycle <- v44.mV <-
    type <- NULL

  if (!quiet)
    glue("Info: generating plot of {length(unique(.data$file_id))} data files, of which {length(unique(pull(filter(.data, cycle_dis), file_id)))} have a drop in pressure.") %>%
            message()
  # we need a weird group for the lines
  pld <- .data %>%
    mutate(grp = paste(file_id, type, sep = "_"),
           linegrps = ifelse(hasdrop,
                             ifelse(cycle_dis, "disabled", "has a drop"),
                             "no bad cycles"))
  # TODO: convert linegrps to factor with desired order?

  pld %>%
    ggplot(aes(x = cycle,
               y = v44.mV,
               group = grp,
               colour = linegrps,
               alpha = linegrps,
               size = linegrps)) +
    # cleanly
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("red", "orange", "gray")) +
    scale_alpha_manual(values = c(.6, .2, .1)) +
    scale_size_manual(values = c(3, 2, 1)) +
    geom_hline(yintercept = range(min, max), col = "indianred", size = 2) +
    facet_grid(cols = vars(type))
}
