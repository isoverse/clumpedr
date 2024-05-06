#' Find bad cycles based on several criteria.
#'
#' This function tries to identify cycles in which the pressure has dropped
#' fast. It first disables all the cycles above or below certain thresholds
#' (`min`, `max`), and then calculates whether the next cycle has dropped by
#' more than `fac` times either the initial (`relative to = "init"`, default)
#' or the previous (`relative_to = "prev"`) cycle.
#'
#' @details The drop in intensity can be defined relative to the first change
#'   in intensity (default), or to the previous cycle.
#'
#' Note that the min, max, and fac input parameters must be columns in the
#' input data, so that reproducibility is guaranteed.
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [isoreader::iso_get_raw_data()].
#' @param min Minimum intensity level for good cycles. Defaults to 1,500 mV.
#' @param max Maximum intensity level for good cycles. Defaults to 50,000 mV.
#' @param fac Factor for how much larger the current drop should be than the
#'   one specified in `relative_to`.
#' @param v44 Column name of mass 44.
#' @param cycle Column name of the column with the measurement cycle number.
#' @param relative_to cycle Drop detection occurs relative to either the first
#'   cycle ("init", default) or to the previous cycle ("prev").
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with some new columns that allow us to disable
#'   certain cycles/whole samples.
#' @examples
#' isoreader::iso_get_raw_data(standards, include_file_info = "Analysis") |>
#'   mutate(dis_min = 500, dis_max = 50000, dis_fac = 3) |>
#'   find_bad_cycles(min = "dis_min", max = "dis_max",
#'                   fac = "dis_fac", relative_to = "init")
#' @export
#' @family cycle functions
find_bad_cycles <- function(.data, ...,
                            min, max, fac,
                            v44 = "v44.mV", cycle = "cycle",
                            relative_to = "init",
                            quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  # check parameter relative_to
  if (!relative_to %in% c("init", "prev")) {
    stop("'relative_to': ", relative_to, " should be eigher 'init' or 'prev'")
  }
  quiet <- check_quiet(quiet)

  out <- .data %>%
    mutate(outlier_cycle_low = .data[[v44]] <= .data[[min]],
           outlier_cycle_high = .data[[v44]] >= .data[[max]],
           cycle_diff = lead(.data[[v44]], default = Inf) - .data[[v44]],
           # so that diffs are only calculated within one column of sample gas/ref gas
          .by = c("file_id", "type"))

  # TODO: rewrite so that relative_to can be a column in original dataframe
  if(relative_to == "init") {
    out <- out |>
          mutate(first_diff_fac = .data[[fac]] *
                   first(.data$cycle_diff[!(.data$outlier_cycle_low | .data$outlier_cycle_high)]),
                 cycle_drop = .data$cycle_diff < .data$first_diff_fac,
                 .by = c("file_id", "type"))
  } else if (relative_to == "prev") {
        # cycle drop is currently NA if the whole  sample was  too low.
    out <- out |>
      mutate(cycle_drop = .data$cycle_diff < .data[[fac]] * lead(.data$cycle_diff),
                 .by = c("file_id", "type"))
  }

  out <- out |>
    # does the measurement have a pressure drop? (works within group)
    mutate(cycle_has_drop = any(.data$outlier_cycle_low | .data$outlier_cycle_high | .data$cycle_drop, na.rm = TRUE),
           # get the first cycle number where the drop occurs
           cycle_drop_num = .data[[cycle]][.data$cycle_drop][1],
           ## disable if the cycle number is bigger than/equal to the cycle drop number
           outlier_cycle_drop = .data$cycle_has_drop & !.data$outlier_cycle_low & !.data$outlier_cycle_high & !is.na(.data$cycle_drop_num) & (.data[[cycle]] >= .data$cycle_drop_num),
           .by = "file_id") %>%
    mutate(outlier_cycle = .data$outlier_cycle_low | .data$outlier_cycle_high | .data$outlier_cycle_drop, .by = "file_id")

  if (!quiet) {
    glue("Info: found {length(unique(pull(filter(out, .data$cycle_has_drop), file_id)))} out of {length(unique(pull(out, file_id)))} acquisitions with a drop in pressure of mass 44.") %>%
      message()
  }

  out
}
