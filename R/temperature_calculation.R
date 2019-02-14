#' temperature_calculation
#'
#' This calculates the temperatures.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from `acid_fractionation()`
#' @param D47 The quoted column name of the Δ47 values to use as input.
#' @seealso revcal tempcal
temperature_calculation <- function(dat, D47 = quo(D47_final), quiet = default(quiet)) {
    if (!quiet)
       message("Info: calculating temperature using default calibration, ignoring uncertainty in the calibration.")
    dat %>%
        mutate(temperature = revcal(D47 = !!D47, ignorecnf = TRUE))
}
