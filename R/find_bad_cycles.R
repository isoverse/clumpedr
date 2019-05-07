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
  v44.mV <- cycle_dis <- v44_diff <- file_id <- v44_diff_init <-
    v44_diff <- v44_drop <- has_drop <- v44_drop_NA <- cycle_drop <- type <- NULL

  v44 <- enquo(v44)
  cycle <- enquo(cycle)

  # check parameter relative_to
  if (!relative_to %in% c("init", "prev"))
    stop("'relative_to': ", relative_to, " should be eigher 'init' or 'prev'")

  # find the extremes
  out <- .data %>%
    mutate(cycle_dis = case_when(!! v44 <= min ~ "low_v44",
                                 !! v44 >= max ~ "high_v44",
                                 TRUE ~ "ok_so_far"))

  # filter them out, then calculate drops
  drp <- out %>%
    filter(cycle_dis == "ok_so_far") %>%
    group_by(file_id, type) %>%
    ## arrange(file_id, type, !! v44) %>%
    mutate(v44_diff = lead(!! v44) - !! v44) %>%
    when(relative_to == "init" ~
           (.) %>% mutate(v44_drop = v44_diff < fac * first(v44_diff)),
         relative_to == "prev" ~
           (.) %>% mutate(v44_drop = v44_diff < fac * lead(v44_diff))) %>%
    # I thought this would be a good replacement for the above, but it doesn't work!
    # this never detects a drop, somehow. Maybe it doesn't do it within groups?
    ## mutate(v44_drop = ifelse(relative_to == "prev",
                             ## v44_diff < fac * lead(v44_diff),
    ## v44_diff < fac * first(v44_diff))) %>%
    # update cycle_dis
    # does the measurement have a pressure drop? (works within group)
    mutate(has_drop = any(v44_drop, na.rm = TRUE)) %>%
    # all the cycles after the first drop get the cycle number(s) where
    # the drop occurs
    mutate(cycle_drop = ifelse(v44_drop, !! cycle, Inf)) %>%
    ## disable if the cycle number is bigger than/equal to the disabled cylce number
    mutate(cycle_dis = case_when(
      v44_drop ~ "pressure_drop",
      has_drop & (!! cycle > cycle_drop | is.na(cycle_drop)) ~ "drop_before",
      TRUE ~ "no_drop"))

  # add it back to cycles with high or low intensities
  out <- out %>%
    filter(cycle_dis != "ok_so_far") %>%
    bind_rows(drp)

  if (!quiet)
    glue("Info: found {nrow(filter(out, cycle_dis != 'no_drop'))}/{length(unique(pull(filter(out, cycle_dis != 'no_drop'), file_id)))} bad cycles/acquisitions out of {nrow(out)}/{length(unique(out$file_id))} total sample gas and working gas cycles/acquisitions.") %>% message()

  out
}
