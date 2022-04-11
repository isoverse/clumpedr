#' Compute \eqn{\delta^{13}}{δ13}C, \eqn{\delta^{18}}{δ18}O \eqn{\Delta_{47}}{Δ47},
#' \eqn{\Delta_{48}}{Δ48}, and \eqn{\Delta_{49}}{Δ49} values
#'
#' Requires a dataframe with little \eqn{\delta}{δ} values, and working gas
#' \eqn{\delta^{13}}{δ13}C and \eqn{\delta^{18}}{δ18}O values.
#'
#' @param .data A [tibble][tibble::tibble-package] containing delta values 45 through 49.
# # ' @param d13C_PDB_wg Column name of reference gas \eqn{\delta^{13}}{δ13}C value.
# #' @param d18O_PDBCO2_wg Column name of reference gas \eqn{\delta^{13}}{δ13}C gas value.
#' @param d45 Column name of d45.
#' @param d46 Column name of d46.
#' @param d47 Column name of d47.
#' @param d48 Column name of d48.
#' @param d49 Column name of d49.
#' @inheritParams isobar_ratios
#' @inheritParams default.params
#' @references Daëron, M., Blamart, D., Peral, M., & Affek, H. P., Absolute
#'   isotopic abundance ratios and the accuracy of \eqn{\Delta_{47}}{Δ47}
#'   measurements, _Chemical Geology_ **2016**, _442_, 83–96.
#'   \doi{10.1016/j.chemgeo.2016.08.014}
#' @export
bulk_and_clumping_deltas  <- function(.data,
                                      # make the column names a bit flexible
                                      ## d13C_PDB_wg=d13C_PDB_wg,
                                      ## d18O_PDBCO2_wg=d18O_PDBCO2_wg,
                                      d45 = d45, d46 = d46, d47 = d47,
                                      d48 = d48, d49 = d49,
                                      # "global" constants
                                      R13_PDB = default(R13_PDB),
                                      R18_PDB = default(R18_PDB),
                                      R17_PDBCO2 = default(R17_PDBCO2),
                                      R18_PDBCO2 = default(R18_PDBCO2),
                                      lambda = default(lambda),
                                      D17O = default(D17O), quiet = default(quiet)) {
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }

  # global variables and defaults
  R18_wg <- R13_wg <- R45_wg <- R46_wg <- R47_wg <- R48_wg <- R49_wg <- R45_stoch <-
    R46_stoch <- R47_stoch <- R48_stoch <- R49_stoch <- NULL

  if (!quiet)
    # d13C and d18O with nice UTF-8 glyphs
    message("Info: calculating \u03b4\u00b9\u00b3C, \u03b4\u00b9\u2078O, and \u0394's.")

  out <- .data %>%
    # scramble the working gas
    mutate(R13_wg  = R13_PDB * (1 + .data$d13C_PDB_wg / 1000),
           R18_wg  = R18_PDBCO2 * (1 + .data$d18O_PDBCO2_wg / 1000)) %>%
    isobar_ratios(R13 = R13_wg, R18 = R18_wg,
                  R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                  R48 = R48_wg, R49 = R49_wg) %>%
    # compute analyte isobar ratios
    mutate(R45 = (1 + {{ d45 }} / 1000) * .data$R45_wg,
           R46 = (1 + {{ d46 }} / 1000) * .data$R46_wg,
           R47 = (1 + {{ d47 }} / 1000) * .data$R47_wg,
           R48 = (1 + {{ d48 }} / 1000) * .data$R48_wg,
           R49 = (1 + {{ d49 }} / 1000) * .data$R49_wg,
           # Solve the generalized form of equation (17) from Brand et al. (2010)
           # [ http://dx.doi.org/10.1351/PAC-REP-09-01-05 ] by assuming that d18O_PDBCO2
           # is small (a few tens of permil) and solving the corresponding second-order
           # Taylor polynomial:
           K = exp(D17O / 1000) * R17_PDBCO2 * R18_PDBCO2 ^ (-lambda),

           A = -3 * .data$K^2 * R18_PDBCO2^(2 * lambda),
           B = 2 * .data$K * .data$R45 * R18_PDBCO2^lambda,
           C = 2 * R18_PDBCO2,
           D = -.data$R46,

           aa = .data$A * lambda * (2 * lambda - 1) + .data$B * lambda * (lambda - 1) / 2,
           bb = 2 * .data$A * lambda + .data$B * lambda + .data$C,
           cc = .data$A + .data$B + .data$C + .data$D,

           d18O_PDBCO2 = 1000 * (-.data$bb + sqrt(.data$bb^2 - 4 * .data$aa * .data$cc )) / (2 * .data$aa),
           d18O_PDB = (.data$d18O_PDBCO2 + 1000) / R18_PDB - 1000,

           R18 = (1 + .data$d18O_PDBCO2 / 1000) * R18_PDBCO2,
           R17 = .data$K * .data$R18^lambda,
           R13 = .data$R45 - 2 * .data$R17,

           d13C_PDB = 1000 * (.data$R13 / R13_PDB - 1)) %>%
  # Compute stochastic isobar ratios of the analyte
  isobar_ratios(R45 = R45_stoch, R46 = R46_stoch,
                R47 = R47_stoch, R48 = R48_stoch,
                R49 = R49_stoch) %>%
  mutate(
    # Check that R45/R45stoch and R46/R46stoch are indistinguishable from 1,
    R45_flag = (.data$R45 / .data$R45_stoch - 1),
    R46_flag = (.data$R46 / .data$R46_stoch - 1),
    # Compute raw clumped isotope anomalies
    D45_raw = 1000 * (.data$R45 / .data$R45_stoch - 1),
    D46_raw = 1000 * (.data$R46 / .data$R46_stoch - 1),
    D47_raw = 1000 * (.data$R47 / .data$R47_stoch - 1),
    D48_raw = 1000 * (.data$R48 /.data$R48_stoch - 1),
    D49_raw = 1000 * (.data$R49 / .data$R49_stoch - 1),
    param_49 = (.data$s49 / .data$s44 - .data$r49 / .data$r44) * 1000#,
    # EXPERIMENTAL NEW STEP! -> undo on [2021-02-12], nobody knows why we do this
    ## D47_raw = D47_raw - D46_raw - D45_raw,
    ## D48_raw = D48_raw - D46_raw * 2
    )

  ## # raise a warning if the corresponding anomalies exceed 0.02 ppm.
  ## # TODO: append warning to specific value!
  ## if (any(out$R45_flag > 2e-8, na.rm = TRUE)) {
  ##   warning("Some R45 / R45_stoch - 1 are large! \n",
  ##           out %>%
  ##             filter(R45_flag) %>%
  ##             mutate(wrong = 1e6 * (R45 / R45_stoch - 1)))
  ## }
  ## if (any(out$R45_flag > 2e-8, na.rm = TRUE)) {
  ##   warning("Some R46 / R46_stoch - 1 are large! \n",
  ##           out %>%
  ##             filter(R46_flag) %>%
  ##             mutate(wrong = 1e6 * (R46 / R46_stoch - 1)))
  ## }

  out %>%
    as_tibble()
}
