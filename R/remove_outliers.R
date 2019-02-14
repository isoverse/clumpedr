#' Remove outliers
#'
#' @details This function removes outliers based on several criteria. 1) The
#'   initial intensity of both sample and reference gas is above 8 V. 2) The
#'   difference in intensity between sample and reference gas is less than 1.2
#'   V. 3) The sample or reference raw \eqn{\Delta_47} value is less than 4 SD
#'   away from the run mean.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from
#'     `collapse_cycles()`.
#' @param info A [tibble][tibble::tibble-package], resulting from
#'     `clean_did_info()`.
#' @param init The minimum initial intensity of mass 44.
#' @param diff The maximum initial difference in intensity of mass 44.
#' @param nsd_off The number of standard deviations away from the median
#'     Preparation of the standards.
#' @param plot_col The column to use for plotting. Defaults to
#'     `quo(D47raw_mean)`.
#' @inheritParams find_outliers
#' @export
remove_outliers <- function(dat, init = 8000, diff = 1200, nsd_off = 4,
                            std_names = paste0("ETH-", 1:3),
                            plot_col = quo(D47raw_mean),
                            quiet = default(quiet), genplot = default(genplot)) {
    if (!quiet)
        glue("Info: Removing aliquots with initial intensity < {init}, difference in initial
                      intensity > {diff}, or {nsd_off} SD's away from the Preparation mean.") %>%
            message()
    out <- dat %>%
        find_outliers(init = init, diff = diff, nsd_off = nsd_off,
                      std_names = std_names)
    if (genplot) {
        pipe_plot(out, plot_outliers, col = plot_col)
    }
    if (!quiet)
        glue("Info: removing {sum(out$outlier)} outliers out of {nrow(out)} samples.") %>%
            message()
    out %>% filter(!outlier)
}
