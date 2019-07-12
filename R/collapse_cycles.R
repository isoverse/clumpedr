#' Collapse the cycles of the raw measurements
#'
#' This collapses the cycles of the raw measurements and calculates averages
#' and standard deviations per aliquot.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from
#'   [bulk_and_clumping_deltas()].
#' @param collapse_key The .key argument that is passed to the nest function. Defaults
#'   to "cycle".
#' @param na.rm a logical value indicating wheter NA values should be stripped
#'   before the computation proceeds.
#' @param cycle_dis_ok String of `cycle_dis` column that are filtered
#'   exclusively for calculation.
#' @export
collapse_cycles <- function(.data,
                            collapse_key = "cycle_data",
                            na.rm = TRUE, quiet = default(quiet)) {
  # global variables and defaults
  file_id <- d45_mean <- d45_sd <- d46_mean <- d46_sd <- d47_mean <- d47_sd <-
    d48_mean <- d48_sd <- d49_mean <- d49_sd <- d18O_PDBCO2_mean <-
      d18O_PDBCO2_sd <- d18O_PDB_mean <- d18O_PDB_sd <- d13C_PDB_mean <-
        d13C_PDB_sd <- d45 <- d46 <- d47 <- d48 <- d49 <- d18O_PDBCO2 <-
          d18O_PDB <-d13C_PDB <- D47_raw_mean <- D47_raw_sd <- D48_raw_mean <-
            D48_raw_sd <- D49_raw_mean <- D49_raw_sd <- N <- D47_raw <-
              D48_raw <- D49_raw <- outlier_cycle <- param_49 <- NULL

  if (!quiet)
    message("Info: collapsing cycles, calculating sample means and standard deviations.")

  # this creates a nice summary for one of the samples
  summarize_mean <- function(.data) {
    .data %>%
      filter(outlier_cycle %in% FALSE) %>%
      select(d45:d49, d18O_PDBCO2, d18O_PDB, d13C_PDB, D47_raw:param_49) %>%
      summarise_all(.funs = list(~ mean(., na.rm = na.rm),
                                 ~ sd(., na.rm = na.rm),
                                 n = length,
                                 sem = ~ sd(., na.rm = na.rm) / sqrt(length(.))))
  }

  .data %>%
    nest(cycle_data = -file_id
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
