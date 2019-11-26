#' Collapse the cycles of the raw measurements
#'
#' This collapses the cycles of the raw measurements and calculates averages
#' and standard deviations per aliquot.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from
#'   [bulk_and_clumping_deltas()].
#' @param ... Columns for the summary report.
#' @param id Index columns that will be excluded from nesting. Defaults to
#'   `file_id`.
#' @param outlier The column containing outlier information.
#' @param funs List of summary functions to apply. Defaults to mean, sd, n,
#'   sem, 95% cl.
#' @param alpha The confidence level for the summary functions.
#' @param na.rm a logical value indicating wheter NA values should be stripped
#'   before the computation proceeds.
#' @export
collapse_cycles <- function(.data,
                            ...,
                            id = c(file_id),
                            outlier = outlier_cycle,
                            funs = NULL,
                            alpha = 0.05,
                            na.rm = TRUE,
                            quiet = default(quiet)) {
  outlier_cycle <- NULL

  if (!quiet)
    message("Info: collapsing cycles, calculating sample summaries.")

  if (is.null(funs)) {
    message("defaulting to mean, sd, n, sem, and 95% cl")
    funs <- list(~ mean(., na.rm = na.rm),
                 ~ sd(., na.rm = na.rm),
                 n = length,
                 sem = ~ sd(., na.rm = na.rm) / sqrt(length(.) - 1),
                 cl = ~ qt((1 - alpha), length(.) - 1) *
                   sd(., na.rm = na.rm) / sqrt(length(.) - 1))
  }

  # this creates a nice summary for one of the samples
  summarize_mean <- function(.data) {
    .data %>%
      filter({{ outlier }} %in% FALSE) %>%
      select(...) %>%
      summarise_all(.funs = funs)
  }

  .data %>%
    # TODO: add an id argument so that I can select multiple columns that aren't nested (i.e. )
    nest(cycle_data = -{{ id }}) %>%
    bind_cols(map_dfr(.$cycle_data, summarize_mean)) %>%
    as_tibble()
}
