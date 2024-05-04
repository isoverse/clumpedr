#' Collapse the cycles of the raw measurements
#'
#' This collapses the cycles of the raw measurements and calculates averages
#' and standard deviations per aliquot.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [bulk_and_clumping_deltas()].
#' @param cols Columns to calculate summaries for.
#' @param id Index columns that will be excluded from nesting. Defaults to`file_id`.
#' @param outlier The column containing outlier information.
#' @param funs List of summary functions to apply. Defaults to mean, sd, n, sem, 95% cl.
#' @param alpha The confidence level for the summary functions.
#' @param na.rm a logical value indicating wheter NA values should be stripped
#'   before the computation proceeds.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
collapse_cycles <- function(.data,
                            ...,
                            cols = c(d13C_PDB, d18O_PDB, D47_raw, D47_final),
                            id = c(file_id),
                            outlier = outlier_cycle,
                            funs = NULL,
                            alpha = 0.05,
                            na.rm = TRUE,
                            quiet = NULL) {
  outlier_cycle <- d13C_PDB <- d18O_PDB <- D47_raw <- D47_final <- NULL
  rlang::check_dots_empty0(...)
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  if (!quiet) {
    message("Info: collapsing cycles, calculating sample summaries.")
  }

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
      select({{ cols }}) %>%
      summarise_all(.funs = funs)
  }

  .data %>%
    # TODO: add an id argument so that I can select multiple columns that aren't nested (i.e. )
    nest(.by = {{id}}, .key = "cycle_data") %>%
    bind_cols(map_dfr(.$cycle_data, summarize_mean)) %>%
    as_tibble()
}

#' nest cycle data
#'
#' Nest the cycle data resulting from previous computations steps.
#'
#' The parameters are all strings with column names. They default to all the
#' columns that have cycle information based on previous computation steps.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [bulk_and_clumping_deltas()].
#' @param ... Additional chacter vectors to nest by.
#' @param masses The masses that are present in the mass spectrometer. Defaults to 44:49 and 54.
#' @param ratios Ratio columns, based on masses.
#' @param outlier_cycles Columns with outlier_cycle information.
#' @param outliers Other outlier-related columns.
#' @param cycle_drop Columns related to detecing cycle drops.
#' @param bgs Backgrounds.
#' @param bg_corrected Background-corrected raw intensities per cup.
#' @param Rs R values.
#' @param deltas delta values.
#' @param isotopes C values.
#' @param params Taylor polynomial parameters.
#' @param stochastic Stochastic R values.
#' @param flags Flag columns.
#' @param Deltas Big delta values.
#' @param p49 Param 49.
#' @inheritParams quiet
#' @export
nest_cycle_data <- function(.data,
                            ...,
                            masses = c(44:49, 54),
                            ratios = c(paste0("s", masses), paste0("r", masses)),
                            outlier_cycles = c(paste0("outlier_cycle_",
                                                      c("low_s44",
                                                        "low_r44",
                                                        "high_s44",
                                                        "high_r44",
                                                        "drop_s44",
                                                        "drop_r44",
                                                        "s44",
                                                        "r44")),
                                               "outlier_cycle"),
                            outliers = #c(paste0("outlier_", c("param49", "flagged")),
                                         "outlier", #)
                            cycle_drop = paste0("cycle_", c("diff_s44",
                                                            "diff_r44",
                                                            "drop_s44",
                                                            "drop_r44",
                                                            "drop_num_s44",
                                                            "drop_num_r44",
                                                            "has_drop_s44",
                                                            "has_drop_r44")), # note that we're leaving cycle_has_drop out!
                            bgs = c(paste0("s44_bg", masses[!masses %in% c(44, 54)]),
                                    paste0("r44_bg", masses[!masses %in% c(44, 54)])),
                            bg_corrected = paste0(ratios[!ratios %in% c("s44", "r44", "r54", "s54")], "_bg"),
                            Rs = c("R45", "R46", "R47", "R48", "R49",
                                   "R45_wg", "R46_wg", "R47_wg", "R48_wg", "R49_wg",
                                   "R13_wg", "R18_wg", "R17", "R18", "R13"),
                            deltas = c(#"d13C_PDB_wg", "d18O_PDBCO2_wg", # these should be the same and thus metadata!
                                       "d13C_PDB", "d18O_PDBCO2", "d18O_PDB",
                                       "d45", "d46", "d47", "d48", "d49"),
                            isotopes = c("C12", "C13", "C16", "C17", "C18",
                                         "C626", "C627", "C628", "C636",
                                         "C637", "C638", "C727", "C728",
                                         "C737", "C738", "C828", "C838"),
                            params = c("K", "A", "B", "C", "D", "aa", "bb", "cc"),
                            stochastic = c("R45_stoch", "R46_stoch",
                                           "R47_stoch", "R48_stoch",
                                           "R49_stoch"),
                            flags = c("R45_flag", "R46_flag"),
                            Deltas = c("D45_raw", "D46_raw", "D47_raw",
                                       "D48_raw", "D49_raw"),
                            p49 = "param_49", quiet = NULL) {

  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  if (is.null(quiet)) {
    quiet <- default(quiet)
  }

  cols <- c("cycle", ..., ratios, outlier_cycles, outliers, cycle_drop, bg_corrected, bgs,
            Rs, deltas, isotopes, params, stochastic, flags, Deltas, p49)

  if (!all(cols %in% colnames(.data))) {
    warning(glue::glue("columns {glue::glue_collapse(cols[!cols %in% colnames(.data)], sep=', ', last=' and ', width = 60)} not found in data"))
  }

  if (!quiet) {
    message(glue::glue("Nesting by {glue::glue_collapse(cols, sep=', ', last=' and ', width = 60)}."))
  }

  .data %>%
    nest(cycle_data = any_of(cols))
}
