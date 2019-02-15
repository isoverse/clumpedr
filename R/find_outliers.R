#' Find outliers
#'
#' This function finds outliers based on several criteria.
#'
#' Here, we define an outlier as a measurement that has:
#' - an initial mass 44 intensity below `init`.
#' - an imbalance between sample and reference gas of more than `diff`.
#' - a clumped value that is more than `nsd_off` standard deviations away from the mean.
#'
#' @param dat A [tibble][tibble::tibble-package].
#' @param parameter parameter description
#' @export
find_outliers <- function(dat, init = 8000, diff = 1200, nsd_off = 4,
                          std_names = paste0("ETH-", 1:3)) {
    means <- dat %>%
        group_by(Preparation, `Identifier 1`) %>%
        summarize(run_mean = mean(D47raw_mean, na.rm = TRUE),
                  run_med = median(D47raw_mean, na.rm = TRUE),
                  run_sd = sd(D47raw_mean, na.rm = TRUE),
                  run_n = n())

    ## average standard run sd
    std_sd <- means %>%
        filter(`Identifier 1` %in% std_names) %>%
        ungroup() %>%
        group_by(Preparation) %>%
        summarize(mean_run_std_sd = mean(run_sd, na.rm = TRUE))

    dat %>%
        ## append run means
        left_join(means, by = c("Preparation", "Identifier 1")) %>%
        left_join(std_sd, by = "Preparation") %>%
                                        # initial sample gas or reference gas too low
        mutate(outlier = ifelse(s44_init <= init | r44_init <= init, TRUE,
                                        # difference in initial intensity too large
                         ifelse(abs(s44_init - r44_init) >= diff, TRUE,
                                        # sample too far off from run mean
                                D47raw_mean < run_med - nsd_off * mean_run_std_sd |
                                D47raw_mean > run_med + nsd_off * mean_run_std_sd |
                                is.na(run_med))))
}
