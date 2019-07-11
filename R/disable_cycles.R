#' Disable cycles that failed
#'
#' Based on several criteria, filter out cycles that show a sudden pressure drop.
#'
#' This function tries to identify cycles in which the pressure has dropped
#' fast. It first disables all the cycles above or below certain thresholds
#' (`min`, `max`), and then calculates whether the next cycle has dropped by
#' more than `fac` times either the initial (`relative to = "init"`, default)
#' or the previous (`relative_to = "prev"`) cycle.
#'
#' @inheritParams find_bad_cycles
#' @family cycle functions
#' @export
disable_cycles  <- function(.data, min = 1500, max = 50000, fac = 1.5,
                            v44 = v44.mV, cycle = cycle,
                            relative_to = "init",
                            genplot = default(genplot), quiet = default(quiet)) {
  # global variables and defaults
  v44.mV <- cycle_dis <- NULL

  v44 <- enquo(v44)
  cycle <- enquo(cycle)

  out <- find_bad_cycles(.data, min = min, max = max, fac = fac,
                         v44 = !! v44, cycle = !! cycle,
                         relative_to = relative_to)

  if (genplot) pipe_plot(out, plot_disabled_cycles, min = min, max = max)

  ## filt <- filter(tmp, v44_low | v44_high | drop_before)

  ## if (!quiet)
  ##   glue("Info: Applying bad cycle filter, keeping {nrow(out)} out of {nrow(filt)} cycles.") %>%
  ##     message()
  out
}
