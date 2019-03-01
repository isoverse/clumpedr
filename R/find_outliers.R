#' Find outliers
#'
#' This function finds outliers based on several criteria.
#'
#' Here, we define an outlier as a measurement that has:
#' - an initial mass 44 intensity below `init`.
#' - an imbalance between sample and reference gas of more than `diff`.
#' - a clumped value that is more than `nsd_off` standard deviations away from the mean.
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param init Initial intensity threshold for mass 44.
#' @param diff Maximum initial difference in mass 44 threshold between standard and sample gas.
#' @param nsd_off Number of standard deviations away from the mean threshold.
#' @param n_id1 Minimum number of aliquots within session to calculate threshold within group.
#' @param std_names Names of the standards used for the correction.
#' @param session Column name that defines correction session.
#' @param id1 Column name of the sample/standard identifier.
#' @export
find_outliers <- function(.data, init = 8000, diff = 1200, nsd_off = 4, n_id1 = 5,
                          std_names = paste0("ETH-", 1:3), session = Preparation,
                          id1 = `Identifier 1`) {
    session <- enquo(session)
    id1 <- enquo(id1)

    # filter out the ones that went very wrong
    out <- .data %>%
      mutate(outlier = case_when(
        # initial sample gas or reference gas too low
        s44_init <= init | r44_init <= init ~ "low_init",
        # difference in initial intensity too large
        abs(s44_init - r44_init) >= diff ~ "diff_init",
        TRUE ~ "ok_so_far"))

    sess_id1 <- out %>%
      filter(outlier == "ok_so_far") %>%
      group_by(!! session, !! id1) %>%
      summarize(sess_id1_mean = mean(D47_raw_mean, na.rm = TRUE),
                sess_id1_med = median(D47_raw_mean, na.rm = TRUE),
                sess_id1_sd = sd(D47_raw_mean, na.rm = TRUE),
                sess_id1_n = n())

    sess <- out %>%
      filter(outlier == "ok_so_far") %>%
      group_by(!! session) %>%
      summarize(sess_mean = mean(D47_raw_mean, na.rm = TRUE),
                sess_med = median(D47_raw_mean, na.rm = TRUE),
                sess_sd = sd(D47_raw_mean, na.rm = TRUE),
                sess_n = n())

    out %>%
      # append id1+session or session means
      left_join(sess_id1, by = c(quo_name(session), quo_name(id1))) %>%
      left_join(sess, by = quo_name(session)) %>%
      # now substitute the ok_so_far ones with potentiall run sd offset criterion
      mutate(
        sess_id1_offset = sess_id1_med - D47_raw_mean,
        sess_offset = abs(sess_med - D47_raw_mean),
        sess_id1_thres = nsd_off * sess_id1_sd,
        sess_thres = nsd_off * sess_sd,
        outlier = ifelse(outlier == "ok_so_far",
                         ifelse(sess_id1_n > n_id1,
                                ifelse(sess_id1_offset > sess_id1_thres, "off_sess_id1_sd", "no_outlier"),
                                ifelse(sess_n > n_id1,
                                       ifelse(sess_offset > sess_thres, "off_sess_sd", "no_outlier"),
                                       "short_run")),
                         outlier))
    # TODO: include outlier filtering based on d13C or d18O offsets, as well as D48/D49?
}
