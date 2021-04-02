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
#' @family cycle functions
find_bad_cycles <- function(.data, min, max, fac,
                            v44 = v44.mV, cycle = cycle,
                            relative_to = relative_to,
                            quiet = default(quiet)) {
  # global variables and defaults
  v44.mV <- NULL

  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  # check parameter relative_to
  ## if (!relative_to %in% c("init", "prev"))
  ##   stop("'relative_to': ", relative_to, " should be eigher 'init' or 'prev'")

  out <- .data %>%
    # so that diffs are only calculated within one column of sample gas/ref gas
    group_by(.data$file_id, .data$type) %>%
    mutate(outlier_cycle_low = {{ v44 }} <= {{min}},
           outlier_cycle_high = {{ v44 }} >= {{max}},
           cycle_diff = lead({{ v44 }}, default = Inf) - {{ v44 }}) %>%
    # TODO: rewrite so that relative_to can be a column in original dataframe
    # probably stop using when?
    when({{relative_to}} == "init" ~
           mutate((.),
                  first_diff_fac = {{fac}} *
                    first(.data$cycle_diff[!(.data$outlier_cycle_low | .data$outlier_cycle_high)]),
                  cycle_drop = .data$cycle_diff < first_diff_fac),
         # cycle drop is currently NA if the whole  sample was  too low.
         {{relative_to}} == "prev" ~
           mutate((.), cycle_drop = .data$cycle_diff < {{fac}} * lead(.data$cycle_diff))
         ) %>%
    # does the measurement have a pressure drop? (works within group)
    mutate(cycle_has_drop = any(.data$outlier_cycle_low | .data$outlier_cycle_high | .data$cycle_drop, na.rm = TRUE),
           # get the first cycle number where the drop occurs
           cycle_drop_num = {{ cycle }}[.data$cycle_drop][1],
           ## disable if the cycle number is bigger than/equal to the cycle drop number
           outlier_cycle_drop = .data$cycle_has_drop & !.data$outlier_cycle_low & !.data$outlier_cycle_high & !is.na(.data$cycle_drop_num) & ({{ cycle }} >= .data$cycle_drop_num)) %>%
    mutate(outlier_cycle = .data$outlier_cycle_low | .data$outlier_cycle_high | .data$outlier_cycle_drop) %>%
    ungroup(.data$file_id, .data$type)

  if (!quiet)
    glue("Info: found {length(unique(pull(filter(out, .data$cycle_has_drop), file_id)))} out of {length(unique(pull(out, file_id)))} acquisitions with a drop in pressure of mass 44.") %>%
      message()

  out
}
