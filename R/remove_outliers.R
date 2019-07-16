#' Remove outliers
#'
#' @details This function removes outliers based on several criteria. 1) The
#'   initial intensity of both sample and reference gas is above 8 V. 2) The
#'   difference in intensity between sample and reference gas is less than 1.2
#'   V. 3) The sample or reference raw \eqn{\Delta_47}{Δ47} value is less than
#'   4 SD away from the preparation mean.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [collapse_cycles()].
#' @param nsd_off The number of standard deviations away from the median
#'   Preparation of the standards.
#' @param D47 The column with \eqn{\Delta_47}{Δ47} values. Defaults to `D47_raw_mean`.
#' @param plot_x The column to use for plotting the x axis.
#' @inheritParams find_outliers
#' @export
remove_outliers <- function(.data, init_low = 8000, init_high = 40000, diff = 1200, nsd_off = 4,
                            std_names = paste0("ETH-", 1:3), D47 = D47_raw,
                            plot_x = file_datetime,
                            session = Preparation,
                            quiet = default(quiet), genplot = default(genplot)) {
  # global variables and defaults
  D47_raw <- file_datetime <- Preparation <- NULL

  if (!quiet)
    glue("Info: identifying aliquots with initial intensity of mass 44 < {init_high}, > {init_low},
          difference in initial intensity > {diff}, or {nsd_off} SD's away from the {quo_name(enquo(session))} mean.") %>%
      message()

  out <- .data %>%
    find_outliers(init_low = init_low, init_high = init_high, diff = diff, nsd_off = nsd_off,
                  D47 = {{ D47 }}, std_names = std_names, session = {{ session }})

  if (genplot) {
    pipe_plot(out, plot_outliers, x = {{ plot_x }}, y = {{ D47 }})
  }

  if (!quiet)
    glue("Info: found {nrow(filter(out, outlier))} outliers out of {nrow(out)} samples.") %>%
      message()
  out
}
