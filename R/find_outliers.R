#' Find outliers
#'
#' This function finds outliers based on several criteria.
#'
#' Here, we define an outlier as a measurement that has:
#' - an initial mass 44 intensity below `init`.
#' - an imbalance between sample and reference gas of more than `diff`.
#' - a clumped value that is more than `nsd_off` standard deviations away from the mean.
#'
#' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
#' @param init_low Minimum initial intensity threshold for mass 44.
#' @param init_high Maximum initial intensity threshold for mass 44.
#' @param nsd_off Number of standard deviations away from the mean threshold.
#' @param n_id1 Minimum number of aliquots within session to calculate threshold within group.
#' @param D47 The column with \eqn{\Delta_{47}}{Δ47} values.
#' @param std_names Names of the standards used for the correction.
#' @param session Column name that defines correction session.
#' @param id1 Column name of the sample/standard identifier.
#' @param diff Maximum initial difference in mass 44 threshold between standard and sample gas.
#' @export
find_outliers <- function(.data, init_low = 8000, init_high=40000, diff = 1200, nsd_off = 4,
                          n_id1 = 5, D47 = D47_raw, #D47_raw_mean,
                          std_names = paste0("ETH-", 1:3),
                          session = Preparation, id1 = `Identifier 1`) {
  # global variables and defaults
  Preparation <- `Identifier 1` <- outlier <- sess_id1_mean <- sess_id1_med <-
    sess_id1_sd <- sess_id1_n <- sess_mean <- sess_med <- sess_sd <- sess_n <-
      sess_id1_offset <- sess_offset <- sess_id1_thres <- sess_thres <-
        D47_raw <- NULL

  D47 <- enquo(D47)
  session <- enquo(session)
  id1 <- enquo(id1)

  # filter out the ones that went very wrong
  out <- .data %>%
    mutate(
      s44_init_low=s44_init <= init_low,
      r44_init_low=r44_init <= init_low,
      s44_init_high=s44_init >= init_high,
      r44_init_high=r44_init >= init_high,
      i44_init_diff=abs(s44_init - r44_init) >= diff,
      outlier_init=s44_init_low | r44_init_low | s44_init_high | r44_init_high | i44_init_diff)

  sess_id1 <- out %>%
    filter(!outlier_init) %>%
    group_by(!! session, !! id1) %>%
    summarize(sess_id1_mean = mean(!! D47, na.rm = TRUE),
              sess_id1_med = median(!! D47, na.rm = TRUE),
              sess_id1_sd = sd(!! D47, na.rm = TRUE),
              sess_id1_n = n())

  sess <- out %>%
    filter(!outlier_init) %>%
    group_by(!! session) %>%
    summarize(sess_mean = mean(!! D47, na.rm = TRUE),
              sess_med = median(!! D47, na.rm = TRUE),
              sess_sd = sd(!! D47, na.rm = TRUE),
              sess_n = n())

  out %>%
    # append id1+session or session means
    left_join(sess_id1, by = c(quo_name(session), quo_name(id1))) %>%
    left_join(sess, by = quo_name(session)) %>%
    # now substitute the ok_so_far ones with potentiall run sd offset criterion
    mutate(
      out_sess_id1_sd=sess_id1_n > n_id1 & abs(sess_id1_med - !! D47) > nsd_off * sess_id1_sd,
      out_sess_sd=sess_n > n_id1 & abs(sess_med - !! D47) > nsd_off * sess_sd,
      outlier_session=out_sess_id1_sd | out_sess_sd,
      outlier=outlier_cycle | outlier_init | outlier_session)
  # TODO: include outlier filtering based on:
  # filter too large internal SD -> set D47 to D47_raw in stead of D47_raw_mean
  # filter d13C or d18O off
  # filter high D48/D49/49_param
}
