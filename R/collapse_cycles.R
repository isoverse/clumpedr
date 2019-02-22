#' Collapse the cycles of the raw measurements
#'
#' This collapses the (filtered) cycles of the raw measurements and calculates
#' averages and standard deviations.
#' @param dat A tibble resulting from `bulk_and_clumping_deltas()`.
#' @param na.rm Logical. Whether or not to remove NA values.
#' @export
collapse_cycles <- function(dat, na.rm = TRUE, quiet = default(quiet)) {
    if (!quiet)
       message("Info: collapsing cycles, calculating sample means and standard deviations.")
    dat %>%
        group_by(file_id) %>%
        filter(!is.na(d45), !is.na(d46), !is.na(d47), !is.na(d48), !is.na(d49), !is.na(D47_raw)) %>%
        summarise(
            d18O_PDBCO2_mean = mean(d18O_PDBCO2, na.rm = na.rm),
            d18O_PDBCO2_sd = sd(d18O_PDBCO2, na.rm = na.rm),
            d18O_PDB_mean = mean(d18O_PDB, na.rm = na.rm),
            d18O_PDB_sd = sd(d18O_PDB, na.rm = na.rm),
            d13C_PDB_mean = mean(d13C_PDB, na.rm = na.rm),
            d13C_PDB_sd = sd(d13C_PDB, na.rm = na.rm),
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
            D47_raw_mean = mean(D47_raw, na.rm = na.rm),
            D47_raw_sd = sd(D47_raw, na.rm = na.rm),
            D48_raw_mean = mean(D48_raw, na.rm = na.rm),
            D48_raw_sd = sd(D48_raw, na.rm = na.rm),
            D49_raw_mean = mean(D49_raw, na.rm = na.rm),
            D49_raw_sd = sd(D49_raw, na.rm = na.rm),
            ## TODO: this n is not calculated correctly now! It should first filter?
            N = n()
        )
}
