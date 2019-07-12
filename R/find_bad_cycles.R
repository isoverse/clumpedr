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
#' @export
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
    mutate(v44_low={{ v44 }} <= min,
           v44_high={{ v44 }} >= max,
           v44_diff=lead({{ v44 }}) - {{ v44 }}) %>%
    when(relative_to == "init" ~
           (.) %>%
           mutate(v44_drop = .data$v44_diff < fac * first(filter(., !.data$v44_low, !.data$v44_high)$v44_diff)),
         relative_to == "prev" ~
           (.) %>%
           mutate(v44_drop = .data$v44_diff < fac * lead(.data$v44_diff))) %>%
    # update cycle_dis
    group_by(.data$file_id, .data$type) %>%
    # does the measurement have a pressure drop? (works within group)
    mutate(has_drop=any(.data$v44_drop, na.rm = TRUE),
    # get the cycle number of where the drop occurs
           cycle_drop=ifelse(.data$v44_drop, {{ cycle }}, Inf),
    ## disable if the cycle number is bigger than/equal to the disabled cylce number
           drop_before=.data$has_drop & ({{ cycle }} >= .data$cycle_drop))
  # add it back to cycles with high or low intensities
           ## cycle_dis=glue("{if(v44_low|is.na(v44_low))'v44_low'}{if(v44_high|is.na(v44_high))'v44_high'}{if(v44_drop|is.na(v44_drop))'v44_drop'}{if(drop_before|is.na(drop_before))'drop_before'}"))

  if (!quiet)
    glue("Info: found {nrow(filter(out, .data$v44_low | .data$v44_high | .data$drop_before))}/{length(unique(pull(filter(out, .data$v44_low | .data$v44_high | .data$drop_before), .data$file_id)))} bad cycles/acquisitions out of {nrow(out)}/{length(unique(out$file_id))} total sample gas and working gas cycles/acquisitions.") %>%
      message()

  out
}
