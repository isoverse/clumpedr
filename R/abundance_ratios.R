#' Calculate abundance ratio based on input intensities
#'
#' Calculate abundance ratios by dividing input mass column \eqn{i} over mass
#' 44. \deqn{R_i = mass_i / mass_{44}}
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param i44 Name of mass 44 column.
#' @param i45 Name of mass 45 column.
#' @param i46 Name of mass 46 column.
#' @param i47 Name of mass 47 column.
#' @param i48 Name of mass 48 column.
#' @param i49 Name of mass 49 column.
#' @param R45 Desired new name of the calculated ratio for mass 45.
#' @param R46 Desired new name of the calculated ratio for mass 46.
#' @param R47 Desired new name of the calculated ratio for mass 47.
#' @param R48 Desired new name of the calculated ratio for mass 48.
#' @param R49 Desired new name of the calculated ratio for mass 49.
abundance_ratios <- function(.data,
                             i44 = s44, i45 = s45,
                             i46 = s46, i47 = s47, i48 = s48,
                             i49 = s49,
                             R45 = R45, R46 = R46, R47 = R47,
                             R48 = R48, R49 = R49, quiet = default(quiet)) {
  # global variables and defaults
  s44 <- s45 <- s46 <- s47 <- s48 <- s49 <- NULL

  if (!quiet)
    message("Info: calculating abundance ratios R[i] = i / 44")

  .data %>%
    mutate({{ R45 }} := {{ i45 }} / {{ i44 }},
           {{ R46 }} := {{ i46 }} / {{ i44 }},
           {{ R47 }} := {{ i47 }} / {{ i44 }},
           {{ R48 }} := {{ i48 }} / {{ i44 }},
           {{ R49 }} := {{ i49 }} / {{ i44 }})
}
