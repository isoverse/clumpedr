#' Remove outliers
#'
#' @details This function removes outliers based on several criteria. 1) The
#'   initial intensity of both sample and reference gas is above 8 V. 2) The
#'   difference in intensity between sample and reference gas is less than 1.2
#'   V. 3) The sample or reference raw \eqn{\Delta_47} value is less than
#'   4 SD away from the preparation mean.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [collapse_cycles()].
#' @param nsd_off The number of standard deviations away from the median
#'   Preparation of the standards.
#' @param D47 The column with \eqn{\Delta_47} values. Defaults to `D47_raw_mean`.
#' @inheritParams find_outliers
#' @export
remove_outliers <- function(.data,
                            ...,
                            init_low = 8000, init_high = 40000, init_diff = 1200,
                            param49_off = 1,
                            internal_sd = 0.15,
                            n_min = 5,
                            n_id1 = 5,
                            nsd_off = 4,
                            D47 = D47_raw, #D47_raw_mean,
                            session = Preparation,
                            id1 = `Identifier 1`,
                            ## plot_x = file_datetime,
                            quiet = default(quiet)) {
  # global variables and defaults
  D47_raw <- file_datetime <- Preparation <- NULL

  out <- .data %>%
    find_outliers(init_low, init_high, init_diff, param49_off, internal_sd,
                            n_min,
                            n_id1,
                            nsd_off,
                            {{ D47 }}, #D47_raw_mean,
                            {{ session }},
                            {{ id1 }}, quiet = quiet)

  if (!quiet)
    glue("Info: found {nrow(filter(out, outlier | is.na(outlier)) %>% select(file_id) %>% distinct())} outliers out of {out %>% select(file_id) %>% distinct() %>% nrow()} measurements.") %>%
      message()

  out
}
