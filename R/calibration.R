#' Temperature calibration
#'
#' A clumped isotope temperature calibration of the form: y = a * 10^6^ / T^2^ + b
#'
#' Defaults to Bonifacie et al. 2017
#' Δ_47_ = (0.0449 ± 0.001 * 10^6^) / T^2^ + (0.167 ± 0.01)
#'
#' @param Tc The temperature in °C.
#' @param slope The slope of the regression.
#' @param intercept The intercept of the regression.
#' @param slpcnf The confidence of the slope of the regression.
#' @param intcnf The confidence of the intercept of the regression.
#' @return A [tibble][tibble::tibble-package] with input Tc and estimated Δ47
#'     value with lower and upper bounds.
#' @seealso revcal
#' @export
tempcal <- function(Tc,
                    slope = 0.0449, intercept = 0.167,
                    slpcnf = 0.001, intcnf = 0.01,
                    ignorecnf = FALSE) {
  if (!is.numeric(Tc)) stop("Tc has to be a numeric")
  # the calibration for one input temperature
  cal <- function(Tc = Tc, slp = slope, int = intercept) {
    (slp * 1e6) / (Tc + kkelvin)^2 + int
  }

  if (ignorecnf) {
    return(cal(Tc, slope, intercept))
  } else {
    # if confidence, return a fancy tibble with means, lower bounds
    # and upper bounds.
    D47 <- tibble(
      Tc = Tc,
      D47 = map_dbl(Tc, cal, slp = slope, int = intercept),
      lwr = map_dbl(Tc, cal, slp = slope - slpcnf, int = intercept - intcnf),
      upr = map_dbl(Tc, cal, slp = slope + slpcnf, int = intercept + intcnf)
    )
    return(D47)
  }
}

#' Reverse temperature calibration
#'
#' A clumped isotope temperature calibration in reversed form, where the normal
#' calibration y = a * 10^6 / T_K^2 + b is expressed as a function of T_C:
#' T_C = sqrt((a * 10^6) / (y - b)) - 273.15
#'
#' Defaults to Bonifacie et al. 2017
#' Δ_47_ = (0.0449 ± 0.001 * 10^6^) / T^2^ + (0.167 ± 0.01)
#'
#' @param D47 The Δ47 value.
#' @param slope The slope of the regression.
#' @param intercept The intercept of the regression.
#' @param slpcnf The confidence of the slope of the regression.
#' @param intcnf The confidence of the intercept of the regression.
#' @return A tibble with input D47 and estimated Tc value with lower and upper bounds.
#' @seealso tempcal
#' @importFrom investr calibrate
#' @export
revcal <- function(D47,
                   slope = 0.0449, intercept = 0.167,
                   slpcnf = 0.001, intcnf = 0.01,
                   ignorecnf = FALSE) {
  if (!is.numeric(D47)) stop("D47 has to be a numeric")
  # the calibration for one input D47
  cal <- function(D47 = D47, slp = slope, int = intercept) {
    sqrt((slp * 1e6) / (D47 - int)) - kkelvin
  }

  # We need some hacky way of converting errors in one direction to erros in
  # the other. Since this is not a linear calibration, we cannot use investr.

  # the calibration for one input temperature
  calT <- function(Tc = Tc, slp = slope, int = intercept) {
    (slp * 1e6) / (Tc + kkelvin)^2 + int
  }
  if (ignorecnf) {
    return(cal(D47, slope, intercept))
  } else {
    Tc <- D47 %>%
      cal(slp = slope, int = intercept) %>% # convert to temperature
      tempcal(
        slope = slope, intercept = intercept,
        slpcnf = slpcnf, intcnf = intcnf
      ) %>% # convert to D47 with errors
      mutate(lwr = lwr %>% cal(), upr = upr %>% cal()) # convert errors to T
    return(Tc)
  }
}
