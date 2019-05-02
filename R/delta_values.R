#' Calculate abundance ratios, \eqn{\delta^{18}}{δ18}O, \eqn{\delta^{13}}{δ13}C, \eqn{\delta}{δ}'s, and raw \eqn{\Delta}{Δ} values
#'
#' @param .data A [tibble][tibble::tibble-package], resulting from
#'   [correct_backgrounds()].
#' @param d13C_PBD_wg Working gas \eqn{\delta^{13}}{δ13}C value.
#' @param d18O_PBDCO2_wg Working gas \eqn{\delta^{18}}{δ18}O value.
#' @param method Method for matching reference gas to sample gas. Either
#'   `"normal"` (default) or `"linterp"`.
#' @param plot_info The needed metadata for plotting. Result of
#'   [clean_did_info()].
#' @param plot_column The column that will be plotted. Defaults to `D47_raw`.
#' @export
delta_values <- function(.data, d13C_PDB_wg = NULL, d18O_PDBCO2_wg = NULL,
                         method = "normal", plot_info = NULL,
                         plot_column = D47_raw, genplot = default(genplot),
                         quiet = default(quiet)) {
  # global variables and defaults
  D47_raw <- R45_wg <- R46_wg <- R47_wg <- R48_wg <- R49_wg <- r44 <- r45 <-
    r46 <- r47 <- r48 <- r49 <- NULL

  plot_column <- enquo(plot_column)

  if (genplot & is.null(plot_info))
    stop("Supply plotting information, generated with `clean_did_info()`")

  # defaults for d13c and d18o of working gas
  if (is.null(d13C_PDB_wg)) {
    # TODO: get d13c ref from raw data file
    d13C_PDB_wg <- -2.820
    if (!quiet)
      glue("Warning: no d13C_PDB_wg value specified, using UU-default of {d13C_PDB_wg}") %>% message()
  }
  if (is.null(d18O_PDBCO2_wg)) {
    # TODO: get d18o ref from raw data file
    d18O_PDBCO2_wg <- -4.670
    if (!quiet)
      glue("Warning: no d18O_PDBCO2_wg value specified, using UU-default of {d18O_PDBCO2_wg}") %>% message()
  }

  out <- .data %>%
    # sample gas
    abundance_ratios() %>%
    # working gas
    abundance_ratios(i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48,
                     i49 = r49, R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                     R48 = R48_wg, R49 = R49_wg) %>%
    little_deltas(quiet = quiet) %>%
    bulk_and_clumping_deltas(d13C_PDB_wg = d13C_PDB_wg,
                             d18O_PDBCO2_wg = d18O_PDBCO2_wg)

  if (genplot)
    pipe_plot(out, plot_raw_delta, .info = plot_info, y = !! plot_column,
              quiet = quiet)

  return(out)
}
