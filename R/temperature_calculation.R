#' temperature_calculation
#'
#' This calculates the temperatures.
#'
#' @param dat A [tibble][tibble::tibble-package], resulting from `acid_fractionation()`
temperature_calculation <- function(dat, quiet = default(quiet)) {
    if (!quiet)
       message("Info: calculating temperature using default calibration, ignoring uncertainty in the calibration.")
    dat %>%
        mutate(temperature = revcal(D47 = D47_final, ignorecnf = TRUE))
}
