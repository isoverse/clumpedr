#' Compute δ^13^C (‰ PDB), δ^18^O (‰ PDB-CO2), Δ~47~, Δ~48~ and Δ~49~ values
#' (‰)
#'
#' Requires a dataframe with little δ values, and working gas δ^13^C
#' and δ^18^O values.
#'
#' @param dat A tibble containing delta values 45 through 49.
#' @param d13C_PDB_wg Reference gas δ^13^C value.
#' @param d18O_PDBCO2_wg Reference gas δ^13^C gas value.
#' @inheritParams isobar_ratios
#' @export
bulk_and_clumping_deltas  <- function(dat, d13C_PDB_wg, d18O_PDBCO2_wg,
                                      # make the column names a bit flexible
                                      d45 = quo(d45), d46 = quo(d46), d47 = quo(d47),
                                      d48 = quo(d48), d49 = quo(d49),
                                      ## "global" constants
                                      R13_PDB = default(R13_PDB),
                                      R18_PDB = default(R18_PDB),
                                      R17_PDBCO2 = default(R17_PDBCO2),
                                      R18_PDBCO2 = default(R18_PDBCO2),
                                      lambda = default(lambda),
                                      D17O = default(D17O), quiet = default(quiet)) {
    if (!quiet)
      message("Info: calculating δ13C, δ18O, and Δ's.")
    # assemble wg into tibble
    wg  <- tibble(d13C_PDB_wg = d13C_PDB_wg, d18O_PDBCO2_wg = d18O_PDBCO2_wg)

    ## scramble the working gas
    wg  <- wg %>%
        mutate(R13_wg  = R13_PDB * (1 + d13C_PDB_wg / 1000),
               R18_wg  = R18_PDBCO2 * (1 + d18O_PDBCO2_wg / 1000)) %>%
        isobar_ratios(R13 = quo(R13_wg), R18 = quo(R18_wg),
                      R45 = quo(R45_wg), R46 = quo(R46_wg), R47 = quo(R47_wg),
                      R48 = quo(R48_wg), R49 = quo(R49_wg))

    ## compute analyte isobar ratios
    dat <-  dat %>%
        mutate(
            R45 = (1 + !!d45 / 1000) * wg$R45_wg,
            R46 = (1 + !!d46 / 1000) * wg$R46_wg,
            R47 = (1 + !!d47 / 1000) * wg$R47_wg,
            R48 = (1 + !!d48 / 1000) * wg$R48_wg,
            R49 = (1 + !!d49 / 1000) * wg$R49_wg)

    ## Solve the generalized form of equation (17) from Brand et al. (2010)
    ## [ http://dx.doi.org/10.1351/PAC-REP-09-01-05 ] by assuming that d18O_PDBCO2
    ## is small (a few tens of permil) and solving the corresponding second-order
    ## Taylor polynomial:

    dat  <- dat %>%
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

    dat <- dat %>%
    ## Compute stochastic isobar ratios of the analyte
        isobar_ratios(R45 = quo(R45_stoch), R46 = quo(R46_stoch),
                      R47 = quo(R47_stoch), R48 = quo(R48_stoch),
                      R49 = quo(R49_stoch))

    ## Check that R45/R45stoch and R46/R46stoch are undistinguishable from 1,
    dat  <- dat %>%
        mutate(R45_flag = (R45 / R45_stoch - 1),
               R46_flag = (R46 / R46_stoch - 1))

    ## and raise a warning if the corresponding anomalies exceed 0.02 ppm.
    if (any(dat$R45_flag > 2e-8, na.rm = TRUE)) {
        warning("Some R45 / R45_stoch - 1 are large! \n",
                dat %>%
                filter(R45_flag) %>%
                mutate(wrong = 1e6 * (R45 / R45_stoch - 1)))
    }
    if (any(dat$R45_flag > 2e-8, na.rm = TRUE)) {
        warning("Some R46 / R46_stoch - 1 are large! \n",
                dat %>%
                filter(R46_flag) %>%
                mutate(wrong = 1e6 * (R46 / R46_stoch - 1)))
    }

    ## Compute raw clumped isotope anomalies
    dat  <- dat %>%
        mutate(D47raw = 1000 * (R47 / R47_stoch - 1),
               D48raw = 1000 * (R48 / R48_stoch - 1),
               D49raw = 1000 * (R49 / R49_stoch - 1))

    ## omit taylor polynomial coefficients
    dat %>%
        select(-c(K, A, B, C, D, aa, bb, cc))
}
