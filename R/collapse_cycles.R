#' Collapse the cycles of the raw measurements
#'
#' This collapses the cycles of the raw measurements and calculates averages
#' and standard deviations per aliquot.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from
#'   [bulk_and_clumping_deltas()].
#' @param cycle_dis_ok String of `cycle_dis` column that are filtered
#'   exclusively for calculation.
#' @param collapse_key The .key argument that is passed to the nest function. Defaults
#'   to "cycle".
#' @param na.rm a logical value indicating wheter NA values should be stripped
#'   before the computation proceeds.
#' @export
collapse_cycles <- function(.data, cycle_dis_ok = "no_drop",
                            collapse_key = "cycle_data",
                            na.rm = TRUE, quiet = default(quiet)) {
  # global variables and defaults
  file_id <- d45_mean <- d45_sd <- d46_mean <- d46_sd <- d47_mean <- d47_sd <-
    d48_mean <- d48_sd <- d49_mean <- d49_sd <- d18O_PDBCO2_mean <-
      d18O_PDBCO2_sd <- d18O_PDB_mean <- d18O_PDB_sd <- d13C_PDB_mean <-
        d13C_PDB_sd <- d45 <- d46 <- d47 <- d48 <- d49 <- d18O_PDBCO2 <-
          d18O_PDB <-d13C_PDB <- D47_raw_mean <- D47_raw_sd <- D48_raw_mean <-
            D48_raw_sd <- D49_raw_mean <- D49_raw_sd <- N <- D47_raw <-
              D48_raw <- D49_raw <- cycle_dis <- param_49 <- NULL

  if (!quiet)
    message("Info: collapsing cycles, calculating sample means and standard deviations.")

  # this creates a nice summary for one of the samples
  summarize_mean <- function(.data) {
    .data %>%
      filter(cycle_dis ==  "no_drop") %>%
      select(d45:d49, d18O_PDBCO2, d18O_PDB, d13C_PDB, D47_raw:param_49) %>%
      summarise_all(.funs = list(~ mean(., na.rm = TRUE),
                                 ~ sd(., na.rm = TRUE),
                                 n = length,
                                 sem = ~ sd(., na.rm = TRUE) / sqrt(length(.))))
  }

  .data %>%
    nest(
      cycle_data = -file_id
      # or do I want really explicit groups?
      ## cycle_intensities = c(r44:s54),
      ## cycle_R = c(R45:R49_wg, R18:R13, R45_stoch:R49_stoch),
      ## cycle_components = C12:C838,
      ## cycle_taylor = K:cc,
      ## cycle_flags = c(R45_flag, R46_flag),
      ## cycle_output = c(d45:d49, d18O_PDBCO2, d18O_PDB, d13C_PDB, D47_raw:param_49)
    ) %>%
    # I now want to get a subset of the nested data once,
    # so filter by cycle_drop == "no_drop"
    bind_cols(map_dfr(.$cycle_data, summarize_mean)
           ## d45_sd = map_dbl(data, ~ sd(.x$d45, na.rm = na.rm)),
           ## d46_mean = map_dbl(data, ~ mean(.x$d45, na.rm = na.rm)),
           ## d46_sd = map_dbl(data, ~ sd(.x$d45, na.rm = na.rm)),
           ## d47_mean = map_dbl(data, ~ mean(.x$d45, na.rm = na.rm)),
           ## d47_sd = map_dbl(data, ~ sd(.x$d45, na.rm = na.rm)),
           ## d48_mean = map_dbl(data, ~ mean(.x$d45, na.rm = na.rm)),
           ## d48_sd = map_dbl(data, ~ sd(.x$d45, na.rm = na.rm)),
           ## d49_mean = map_dbl(data, ~ mean(.x$d45, na.rm = na.rm)),
           ## d49_sd = map_dbl(data, ~ sd(.x$d45, na.rm = na.rm)),
           ## d18O_PDBCO2_mean = map_dbl(data, ~ mean(.x$d18O_PDBCO2, na.rm = na.rm)),
           ## d18O_PDBCO2_sd = map_dbl(data, ~ sd(.x$d18O_PDBCO2, na.rm = na.rm)),
           ## d18O_PDB_mean = map_dbl(data, ~ mean(.x$d18O_PDB, na.rm = na.rm)),
           ## d18O_PDB_sd = map_dbl(data, ~ sd(.x$d18O_PDB, na.rm = na.rm)),
           ## d13C_PDB_mean = map_dbl(data, ~ mean(.$d13C_PDB, na.rm = na.rm)),
           ## d13C_PDB_sd = map_dbl(data, ~ sd(.x$d13C_PDB, na.rm = na.rm)),

           ## D47_raw_mean = map_dbl(data, ~ mean(.$D47_raw, na.rm = na.rm)),
           ## D47_raw_sd = map_dbl(data, ~ sd(.x$D47_raw, na.rm = na.rm)),
           ## D48_raw_mean = map_dbl(data, ~ mean(.$D48_raw, na.rm = na.rm)),
           ## D48_raw_sd = map_dbl(data, ~ sd(.x$D48_raw, na.rm = na.rm)),
           ## D49_raw_mean = map_dbl(data, ~ mean(.$D49_raw, na.rm = na.rm)),
           ## D49_raw_sd = map_dbl(data, ~ sd(.x$D49_raw, na.rm = na.rm)),
           ## N = map_int(data, ~ nrow(.x))
           )
}
