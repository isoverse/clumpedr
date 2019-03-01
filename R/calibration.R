#' Temperature calibration
#'
#' A clumped isotope temperature calibration of the form:
#' \deqn{y = a \times 10^6 / T^2 + b}{y = a * 10^6 / T^2 + b}
#'
#' Defaults to Bonifacie et al. 2017
#' \deqn{\Delta_47 = (0.0449 \pm 0.001 \times 10^6) / T^2 + (0.167 \pm 0.01)}{Δ47 = (0.0499 ± 0.001 * 10^6) / T^2 + (0.167 ± 0.01)}
#'
#' @param Tc The temperature in °C.
#' @param slope The slope of the regression.
#' @param intercept The intercept of the regression.
#' @param slpcnf The confidence of the slope of the regression.
#' @param intcnf The confidence of the intercept of the regression.
#' @param ignorecnf Whether or not to ignore the confidence of the temperature
#'   calibration.
#' @return A [tibble][tibble::tibble-package] with input Tc and estimated
#'   \eqn{\Delta_47}{Δ47} value with lower and upper bounds.
#' @seealso revcal
#' @export
#' @references M. Bonifacie, D. Calmels, J. M. Eiler, J. Horita, C. Chaduteau,
#'   C. Vasconcelos, P. Agrinier, A. Katz, B. H. Passey, J. M. Ferry, J.
#'   Bourrand. Calibration of the dolomite clumped isotope thermometer from 25
#'   to 350 °C, and implications for a universal calibration for all (Ca, Mg,
#'   Fe)CO\eqn{{}_3}{3} carbonates. **2017**, _200_, 255--279.
tempcal <- function(Tc,
                    slope = 0.0449, intercept = 0.167,
                    slpcnf = 0.001, intcnf = 0.01,
                    ignorecnf = FALSE) {
  kkelvin <- 273.15 # 0 °C
  if (!is.numeric(Tc)) stop("Tc has to be a numeric")
  # the calibration for one input temperature
  cal <- function(Tc = Tc, slp = slope, int = intercept) {
    (slp * 1e6) / (Tc + kkelvin)^2 + int
  }

  if (ignorecnf) {
    return(cal(Tc, slope, intercept))
  } else {
    # if confidence, return a fancy tibble with means, lower bounds and
    # upper bounds.
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
#' calibration
#' \deqn{y = a \times 10^6 / T_K^2 + b}{y = a * 10^6 / T_K^2 + b}
#' is expressed as a function of \eqn{T_C}:
#' \deqn{T_C = sqrt((a \times 10^6) / (y - b)) - 273.15}
#'
#' Defaults to Bonifacie et al. 2017 \deqn{\Delta_47 = (0.0449 \pm 0.001 \times
#' 10^6) / T^2 + (0.167 \pm 0.01)}{Δ47 = (0.0499 ± 0.001 * 10^6) / T^2 + (0.167
#' ± 0.01)}
#'
#' @param D47 The \eqn{\Delta_{47}}{Δ47} value.
#' @inheritParams tempcal
#' @return A tibble with input \eqn{\Delta_{47}}{Δ47} and estimated Tc value
#'   with lower and upper bounds.
#' @seealso tempcal
#' @export
revcal <- function(D47,
                   slope = 0.0449, intercept = 0.167,
                   slpcnf = 0.001, intcnf = 0.01,
                   ignorecnf = FALSE) {
  kkelvin <- 273.15 # 0 °C
  if (!is.numeric(D47)) stop("D47 has to be a numeric")
  # the calibration for one input D47
  cal <- function(D47 = D47, slp = slope, int = intercept) {
    sqrt((slp * 1e6) / (D47 - int)) - kkelvin
  }

  # We need some hacky way of converting errors in one direction to erros in
  # the other. Since this is not a linear calibration, we cannot use
  # investr.

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
