#' Find bad cycles based on several criteria.
#'
#' This filters out any cycles that:
#' - have an initial intensity of mass 44 that is lower than `min`.
#' - have an initial intensity of mass 44 that is larger than `max`.
#' - has a sudden drop in intensity of mass 44
#'
#' @details The drop in intensity can be defined relative to the first change
#'   in intensity (default), or to the previous cycle.
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
find_bad_cycles <- function(.data, min = 1500, max = 50000, fac = 1.5,
                            v44 = v44.mV, cycle = cycle,
                            relative_to = "init",
                            quiet = default(quiet)) {
  # global variables and defaults
  v44.mV <- NULL

  # check parameter relative_to
  if (!relative_to %in% c("init", "prev"))
    stop("'relative_to': ", relative_to, " should be eigher 'init' or 'prev'")

  # find the extremes
  out <- .data %>%
    # first group by file_id and type so that the diffs are calculated only within a single line
    group_by(.data$file_id, .data$type) %>%
    mutate(v44_low = {{ v44 }} <= min,
           v44_high = {{ v44 }} >= max,
           v44_diff = lead({{ v44 }}, default = Inf) - {{ v44 }}) #%>%

  # this needs to be done separately so that we can first filter out the high and low intensities
  drop <- out %>%
    # TODO: come up with a different way of selecting the first value that
    # isn't high or low without filtering within many groups
    filter(!.data$v44_low, !.data$v44_high) %>%
    when(relative_to == "init" ~
           (.) %>%
           mutate(v44_drop = .data$v44_diff < fac * first(.$v44_diff)),
         relative_to == "prev" ~
           (.) %>%
           mutate(v44_drop = .data$v44_diff < fac * lead(.data$v44_diff))) %>%
        mutate(v44_drop = .data$v44_diff < fac * first(.$v44_diff)) %>%
    select(.data$file_id, .data$type, v44_drop, .data$cycle) %>%
    ungroup(.date$file_id, .data$type)

  out <- out %>%
    # add it so that we have all
    left_join(drop, by = c("file_id", "type", "cycle")) %>%
    # does the measurement have a pressure drop? (works within group)
    mutate(has_drop = any(.data$v44_low | .data$v44_high | .data$v44_drop, na.rm = TRUE),
    # get the cycle number of where the drop occurs
           cycle_drop = ifelse(.data$v44_drop, {{ cycle }}, Inf),
    ## disable if the cycle number is bigger than/equal to the disabled cylce number
           drop_before = .data$has_drop & ({{ cycle }} >= .data$cycle_drop)) %>%
    mutate(outlier_cycle = v44_low | v44_high | v44_drop | drop_before) %>%
    ungroup(.date$file_id, .data$type)

  if (!quiet)
    glue("Info: found {length(unique(pull(filter(out, .data$has_drop), file_id)))} out of {length(unique(pull(out, file_id)))} acquisitions with a drop in pressure of mass 44.") %>%
      message()

  out
}
