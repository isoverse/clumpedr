#' Collapse the cycles of the raw measurements
#'
#' This collapses the cycles of the raw measurements and calculates averages
#' and standard deviations per aliquot.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from
#'   [bulk_and_clumping_deltas()].
#' @param na.rm a logical value indicating wheter NA values should be stripped
#'   before the computation proceeds.
#' @export
collapse_cycles <- function(.data, na.rm = TRUE, quiet = default(quiet)) {
  if (!quiet)
    message("Info: collapsing cycles, calculating sample means and standard deviations.")

  # this creates a nice summary for one of the samples
  summarize_mean <- function(.data) {
    .data %>%
      filter(.data$outlier_cycle %in% FALSE) %>%
      select(.data$d45:.data$d49, .data$d18O_PDBCO2, .data$d18O_PDB,
             .data$d13C_PDB, .data$D47_raw:.data$param_49) %>%
      summarise_all(.funs = list(~ mean(., na.rm = na.rm),
                                 ~ sd(., na.rm = na.rm),
                                 n = length,
                                 sem = ~ sd(., na.rm = na.rm) / sqrt(length(.))))
  }

  .data %>%
    nest(cycle_data = -.data$file_id
      # TODO: or do I want really explicit groups?
      ## cycle_intensities = c(r44:s54),
      ## cycle_R = c(R45:R49_wg, R18:R13, R45_stoch:R49_stoch),
      ## cycle_components = C12:C838,
      ## cycle_taylor = K:cc,
      ## cycle_flags = c(R45_flag, R46_flag),
      ## cycle_output = c(d45:d49, d18O_PDBCO2, d18O_PDB, d13C_PDB, D47_raw:param_49)
    ) %>%
    # I now want to get a subset of the nested data once,
    # so filter by no bad cycle
    bind_cols(map_dfr(.$cycle_data, summarize_mean)) %>%
    as_tibble()
}
