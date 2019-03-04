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
  # global variables and defaults
  d45_mean <- d45_sd <- d46_mean <- d46_sd <- d47_mean <- d47_sd <-
    d48_mean <- d48_sd <- d49_mean <- d49_sd <- d18O_PDBCO2_mean <-
      d18O_PDBCO2_sd <- d18O_PDB_mean <- d18O_PDB_sd <- d13C_PDB_mean <-
        d13C_PDB_sd <- d45 <- d46 <- d47 <- d48 <- d49 <- d18O_PDBCO2 <-
          d18O_PDB <-d13C_PDB <- D47_raw_mean <- D47_raw_sd <- D48_raw_mean <-
            D48_raw_sd <- D49_raw_mean <- D49_raw_sd <- N <- D47_raw <-
              D48_raw <- D49_raw <- NULL

  if (!quiet)
    message("Info: collapsing cycles, calculating sample means and standard deviations.")
  .data %>%
    group_by(file_id) %>%
    summarise(
      d45_mean = mean(d45, na.rm = na.rm),
      d45_sd = sd(d45, na.rm = na.rm),
      d46_mean = mean(d46, na.rm = na.rm),
      d46_sd = sd(d46, na.rm = na.rm),
      d47_mean = mean(d47, na.rm = na.rm),
      d47_sd = sd(d47, na.rm = na.rm),
      d48_mean = mean(d48, na.rm = na.rm),
      d48_sd = sd(d48, na.rm = na.rm),
      d49_mean = mean(d49, na.rm = na.rm),
      d49_sd = sd(d49, na.rm = na.rm),

      d18O_PDBCO2_mean = mean(d18O_PDBCO2, na.rm = na.rm),
      d18O_PDBCO2_sd = sd(d18O_PDBCO2, na.rm = na.rm),
      d18O_PDB_mean = mean(d18O_PDB, na.rm = na.rm),
      d18O_PDB_sd = sd(d18O_PDB, na.rm = na.rm),
      d13C_PDB_mean = mean(d13C_PDB, na.rm = na.rm),
      d13C_PDB_sd = sd(d13C_PDB, na.rm = na.rm),

      D47_raw_mean = mean(D47_raw, na.rm = na.rm),
      D47_raw_sd = sd(D47_raw, na.rm = na.rm),
      D48_raw_mean = mean(D48_raw, na.rm = na.rm),
      D48_raw_sd = sd(D48_raw, na.rm = na.rm),
      D49_raw_mean = mean(D49_raw, na.rm = na.rm),
      D49_raw_sd = sd(D49_raw, na.rm = na.rm),

      # TODO: this n is not calculated correctly now! It should first filter?
      N = n()
    )
}
