#' Calculate abundance ratios, \eqn{\delta^{18}}O, \eqn{\delta^{13}}C,
#' \eqn{\delta}'s, and raw \eqn{\Delta} values
#'
#' This is a wrapper function that calculates all delta values.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from [correct_backgrounds()].
#' @param d13C_PBD_wg Working gas \eqn{\delta^{13}}C value.
#' @param d18O_PBDCO2_wg Working gas \eqn{\delta^{18}}O value.
#' @param method Method for matching reference gas to sample gas. Defaults to "linterp".
#' @param plot_info The needed metadata for plotting. Result of [clean_did_info()].
#' @param plot_column The quoted column that will be plotted. Defaults to `quo(D47raw)`.
#' @export
delta_values <- function(dat, d13C_PDB_wg = NULL, d18O_PDBCO2_wg = NULL,
                         method = "linterp",
                         genplot = default(genplot), quiet = default(quiet),
                         plot_info = NULL, plot_column = quo(D47raw)) {
    if (genplot & is.null(plot_info))
        stop("Supply plotting information, generated with `clean_did_info()`")
    ## defaults for d13c and d18o of working gas
    if (is.null(d13C_PDB_wg)) {
        d13C_PDB_wg = -2.68
        if (!quiet)
            glue("Warning: no d13C_PDB_wg value specified, using UU-default of {d13C_PDB_wg}") %>% message()
    }
    if (is.null(d18O_PDBCO2_wg)) {
        d18O_PDBCO2_wg = -4.86
        if (!quiet)
            glue("Warning: no d18O_PDBCO2_wg value specified, using UU-default of {d18O_PDBCO2_wg}") %>% message()
    }

    out <- dat %>%
        ## sample gas
        abundance_ratios() %>%
        ## working gas
        abundance_ratios(i44 = quo(r44), i45 = quo(r45), i46 = quo(r46),
                         i47 = quo(r47), i48 = quo(r48), i49 = quo(r49),
                         R45 = quo(R45_wg), R46 = quo(R46_wg), R47 = quo(R47_wg),
                         R48 = quo(R48_wg), R49 = quo(R49_wg)) %>%
        little_deltas(quiet = quiet) %>%
        bulk_and_clumping_deltas(d13C_PDB_wg = d13C_PDB_wg, d18O_PDBCO2_wg = d18O_PDBCO2_wg)
    if (genplot)
        pipe_plot(out, plot_raw_delta, info = plot_info, column = plot_column, quiet = quiet)
    out
}
