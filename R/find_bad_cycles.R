#' Find bad cycles based on several criteria.
#'
#' This filters out any cycles that:
#' - have an initial intensity of mass 44 that is lower than `min`.
#' - have an initial intensity of mass 44 that is larger than `max`.
#' - has a sudden drop in intensity of mass 44
#'
#' @details The drop in intensity can be defined relative to the first change
#'   in intensity (default), or to the previous cycle.
#' @param dat A [tibble][tibble::tibble-package], resulting from
#'   `isoreader::iso_get_raw_data()`
#' @param min Minimum intensity level for good cycles. Defaults to 1,500 mV.
#' @param max Maximum intensity level for good cycles. Defaults to 50,000 mV.
#' @param fac Factor for how much larger the current drop should be than the
#'   one specified in `relative_to`.
#' @param relative_to cycle Drop detection occurs relative to either the first
#'   cycle ("init", default) or to the previous cycle ("prev").
#' @param m44.mv quoted column name of mass 44.
#' @param cycle quoted column name of the column with the measurement cycle
#'   number.
#' @family cycle functions
#' @export
find_bad_cycles <- function(dat, min = 1500, max = 50000, fac = 1.5,
                            m44.mV = quo(v44.mV), cycle = quo(cycle),
                            relative_to = "init",
                            quiet = default(quiet)) {
      ## check parameter relative_to
      if (!relative_to %in% c("init", "prev"))
          stop("'relative_to': ", relative_to, " should be eigher 'init' or 'prev'")

      out <- dat %>%
          mutate(cycle_dis = is.na(!!cycle) | !!m44.mV <= min | !!m44.mV >= max) %>%
          mutate(v44_diff = ifelse(!cycle_dis, lead(!!m44.mV) - !!m44.mV, NA_real_)) %>%
          group_by(file_id, type) %>%
          when(
              ## filter out those cycles where the difference is larger than the
              ## initial difference
              relative_to == "init" ~
                  (.) %>% mutate(v44_diff_init = head(v44_diff, n = 1)) %>%
                  mutate(v44_drop = v44_diff < fac * v44_diff_init),
              ## rapid intensity drops relative to previous drop
              relative_to == "prev" ~
                  (.) %>% mutate(v44_drop = fac * lead(i44_diff) > i44_diff)) %>%
          mutate(hasdrop = any(v44_drop, na.rm = TRUE)) %>%
          ## deal with NAs
          mutate(v44_drop_NA = ifelse(is.na(v44_drop) & hasdrop, TRUE,
                               ifelse(is.na(v44_drop) & !hasdrop, FALSE, v44_drop))) %>%
          ## all the cycles after the first drop get the cycle number(s) where
          ## the drop occurs
          mutate(cycle_drop = ifelse(v44_drop_NA, cycle, Inf)) %>%
          ## disable if the cycle number is bigger than the disabled cylce number
          mutate(cycle_dis = ifelse(!cycle_dis, cycle >= cycle_drop, cycle_dis)) #%>%
        ## group_by(file_id, type) %>%
        ## mutate(hasdrop = any(cycle_dis)) %>%
        ## ungroup()

    if (!quiet)
        glue("Info: found {sum(out$cycle_dis)} bad cycles out of {nrow(out)} total sample gas and working gas cycles.") %>% message()
    out
}
