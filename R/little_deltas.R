#' Calculate little deltas from abundance ratios
#'
#' For all intensities \eqn{i},
#' \deqn{\delta_i = (R_i / R_iwg - 1) \times 1000}
#'
#' @param dat A dataframe with abundance ratios.
#' @export
little_deltas <- function(dat, quiet = default(quiet)) {
    if (!quiet)
       message("Info: calculating \u03b4 values with (Ri / Ri_wg - 1) * 1000")
    dat %>%
        mutate(
            d45 = (R45 / R45_wg - 1) * 1000,
            d46 = (R46 / R46_wg - 1) * 1000,
            d47 = (R47 / R47_wg - 1) * 1000,
            d48 = (R48 / R48_wg - 1) * 1000,
            d48 = (R48 / R48_wg - 1) * 1000,
            d49 = (R49 / R49_wg - 1) * 1000)
}
