#' Compute \eqn{\delta^{13}}{δ13}C, \eqn{\delta^{18}}{δ18}O \eqn{\Delta_{47}}{Δ47},
#' \eqn{\Delta_{48}}{Δ48}, and \eqn{\Delta_{49}}{Δ49} values
#'
#' Requires a dataframe with little \eqn{\delta}{δ} values, and working gas
#' \eqn{\delta^{13}}{δ13}C and \eqn{\delta^{18}}{δ18}O values.
#'
#' @param .data A [tibble][tibble::tibble-package] containing delta values 45 through 49.
#' @param d13C_PDB_wg Reference gas \eqn{\delta^{13}}{δ13}C value.
#' @param d18O_PDBCO2_wg Reference gas \eqn{\delta^{13}}{δ13}C gas value.
#' @param d45 Column name of d45.
#' @param d46 Column name of d46.
#' @param d47 Column name of d47.
#' @param d48 Column name of d48.
#' @param d49 Column name of d49.
#' @inheritParams isobar_ratios
#' @inheritParams default.params
#' @export
bulk_and_clumping_deltas  <- function(.data, d13C_PDB_wg, d18O_PDBCO2_wg,
                                      # make the column names a bit flexible
                                      d45 = d45, d46 = d46, d47 = d47,
                                      d48 = d48, d49 = d49,
                                      # "global" constants
                                      R13_PDB = default(R13_PDB),
                                      R18_PDB = default(R18_PDB),
                                      R17_PDBCO2 = default(R17_PDBCO2),
                                      R18_PDBCO2 = default(R18_PDBCO2),
                                      lambda = default(lambda),
                                      D17O = default(D17O), quiet = default(quiet)) {
  # global variables and defaults
  K <- A <- B <- C <- D <- aa <- bb <- cc <- R18 <- R18_wg <- R17 <- R13 <-
    R13_wg <- R45_flag <- R46_flag <- D47_raw <- D48_raw <- D49_raw <- R45 <-
      R46 <- R47 <- R48 <- R48 <- d13C_PDB_wg <- d18O_PDBCO2_wg <-
        d18O_PDBCO2 <- d18O_PDB <- R45 <- R46 <- R47 <- R48 <- R49 <-
          R45_stoch <- R46_stoch <- R47_stoch <- R48_stoch <- R49_stoch <-
            R45_wg <- R46_wg <- R47_wg <- R48_wg <- R49_wg <-NULL

  if (!quiet)
    # d13C and d18O with nice UTF-8 glyphs
    message("Info: calculating \u03b4\u00b9\u00b3C, \u03b4\u00b9\u2078O, and \u0394's.")

  d45 <- enquo(d45)
  d46 <- enquo(d46)
  d47 <- enquo(d47)
  d48 <- enquo(d48)
  d49 <- enquo(d49)

  # assemble wg into tibble
  wg  <- tibble(d13C_PDB_wg = d13C_PDB_wg, d18O_PDBCO2_wg = d18O_PDBCO2_wg)

  # scramble the working gas
  wg  <- wg %>%
    mutate(R13_wg  = R13_PDB * (1 + d13C_PDB_wg / 1000),
           R18_wg  = R18_PDBCO2 * (1 + d18O_PDBCO2_wg / 1000)) %>%
    isobar_ratios(R13 = R13_wg, R18 = R18_wg,
                  R45 = R45_wg, R46 = R46_wg, R47 = R47_wg,
                  R48 = R48_wg, R49 = R49_wg)

  # compute analyte isobar ratios
  out <- .data %>%
    mutate(R45 = (1 + !! d45 / 1000) * wg$R45_wg,
           R46 = (1 + !! d46 / 1000) * wg$R46_wg,
           R47 = (1 + !! d47 / 1000) * wg$R47_wg,
           R48 = (1 + !! d48 / 1000) * wg$R48_wg,
           R49 = (1 + !! d49 / 1000) * wg$R49_wg)

  # Solve the generalized form of equation (17) from Brand et al. (2010)
  # [ http://dx.doi.org/10.1351/PAC-REP-09-01-05 ] by assuming that d18O_PDBCO2
  # is small (a few tens of permil) and solving the corresponding second-order
  # Taylor polynomial:

  out  <- out %>%
        mutate(K = exp(D17O / 1000) * R17_PDBCO2 * R18_PDBCO2 ^ (-lambda),

               A = -3 * K^2 * R18_PDBCO2^(2 * lambda),
               B = 2 * K * R45 * R18_PDBCO2^lambda,
               C = 2 * R18_PDBCO2,
               D = -R46,

               aa = A * lambda * (2 * lambda - 1) + B * lambda * (lambda - 1) / 2,
               bb = 2 * A * lambda + B * lambda + C,
               cc = A + B + C + D,

               d18O_PDBCO2 = 1000 * (-bb + sqrt(bb^2 - 4 * aa * cc )) / (2 * aa),
               d18O_PDB = (d18O_PDBCO2 + 1000) / R18_PDB - 1000,

               R18 = (1 + d18O_PDBCO2 / 1000) * R18_PDBCO2,
               R17 = K * R18^lambda,
               R13 = R45 - 2 * R17,

               d13C_PDB = 1000 * (R13 / R13_PDB - 1))

  out <- out %>%
    # Compute stochastic isobar ratios of the analyte
    isobar_ratios(R45 = R45_stoch, R46 = R46_stoch,
                  R47 = R47_stoch, R48 = R48_stoch,
                  R49 = R49_stoch)

  # Check that R45/R45stoch and R46/R46stoch are indistinguishable from 1,
  out  <- out %>%
    mutate(R45_flag = (R45 / R45_stoch - 1),
           R46_flag = (R46 / R46_stoch - 1))

  # and raise a warning if the corresponding anomalies exceed 0.02 ppm.
  # TODO: append warning to specific value!
  if (any(out$R45_flag > 2e-8, na.rm = TRUE)) {
    warning("Some R45 / R45_stoch - 1 are large! \n",
            out %>%
              filter(R45_flag) %>%
              mutate(wrong = 1e6 * (R45 / R45_stoch - 1)))
  }
  if (any(out$R45_flag > 2e-8, na.rm = TRUE)) {
    warning("Some R46 / R46_stoch - 1 are large! \n",
            out %>%
              filter(R46_flag) %>%
              mutate(wrong = 1e6 * (R46 / R46_stoch - 1)))
  }

  # Compute raw clumped isotope anomalies
  out  <- out %>%
    mutate(D47_raw = 1000 * (R47 / R47_stoch - 1),
           D48_raw = 1000 * (R48 / R48_stoch - 1),
           D49_raw = 1000 * (R49 / R49_stoch - 1))

  # omit taylor polynomial coefficients ?
  ## out %>%
    ## select(-c(K, A, B, C, D, aa, bb, cc))
  out
}
