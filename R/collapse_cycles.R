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
        summarise(
            d18O_PDBCO2_mean = mean(d18O_PDBCO2, na.rm = na.rm),
            d18O_PDBCO2_sd = sd(d18O_PDBCO2, na.rm = na.rm),
            d18O_PDB_mean = mean(d18O_PDB, na.rm = na.rm),
            d18O_PDB_sd = sd(d18O_PDB, na.rm = na.rm),
            d13C_PDB_mean = mean(d13C_PDB, na.rm = na.rm),
            d13C_PDB_sd = sd(d13C_PDB, na.rm = na.rm),
            d45= mean(d45, na.rm = na.rm),
            d45_sd = sd(d45, na.rm = na.rm),
            d46_mean = mean(d46, na.rm = na.rm),
            d46_sd = sd(d46, na.rm = na.rm),
            d47_mean = mean(d47, na.rm = na.rm),
            d47_sd = sd(d47, na.rm = na.rm),
            d48_mean = mean(d48, na.rm = na.rm),
            d48_sd = sd(d48, na.rm = na.rm),
            d49_mean = mean(d49, na.rm = na.rm),
            d49_sd = sd(d49, na.rm = na.rm),
            D47raw_mean = mean(D47raw, na.rm = na.rm),
            D47raw_sd = sd(D47raw, na.rm = na.rm),
            D48raw_mean = mean(D48raw, na.rm = na.rm),
            D48raw_sd = sd(D48raw, na.rm = na.rm),
            D49raw_mean = mean(D49raw, na.rm = na.rm),
            D49raw_sd = sd(D49raw, na.rm = na.rm)
        )
}
