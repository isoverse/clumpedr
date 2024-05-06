#' Calculate abundance ratio based on input intensities
#'
#' Calculate abundance ratios by dividing input mass column \eqn{i} over mass
#' 44. \deqn{R_i = mass_i / mass_{44}}
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param i44,i45,i46,i47,i48,i49 Name of mass x column. Defaults to `sx`.
#' @param R45,R46,R47,R48,R49 Desired new name of the calculated ratio for mass x. Defaults to `Rx`.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @returns Same as `.data` but with new columns Rx.
#' @export
#' @examples
#' # easily generate some artificial data
#' x <- function() { rnorm(n = 10) }
#' dat <- tibble::tibble(s44 = x(), s45 = x(), s46 = x(), s47 = x(), s48 = x(), s49 = x(),
#'                       r44 = x(), r45 = x(), r46 = x(), r47 = x(), r48 = x(), r49 = x())
#' dat <- dat |>
#'   abundance_ratios()
#' # to specify the column names, for example when we calculate the bg ratios
#' dat <- dat |>
#'   abundance_ratios(i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48, i49 = r49,
#'                   R45 = R45_wg, R46 = R46_wg, R47 = R47_wg, R48 = R48_wg, R49 = R49_wg)
abundance_ratios <- function(.data,
                             ...,
                             i44 = s44, i45 = s45,
                             i46 = s46, i47 = s47, i48 = s48,
                             i49 = s49,
                             R45 = R45, R46 = R46, R47 = R47,
                             R48 = R48, R49 = R49,
                             quiet = NULL) {
  rlang::check_dots_empty0(...)
  # global variables and defaults
  s44 <- s45 <- s46 <- s47 <- s48 <- s49 <- NULL

  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  quiet <- check_quiet(quiet)

  if (!quiet) {
    message("Info: calculating abundance ratios R[i] = i / 44")
  }

  .data %>%
    mutate({{ R45 }} := {{ i45 }} / {{ i44 }},
           {{ R46 }} := {{ i46 }} / {{ i44 }},
           {{ R47 }} := {{ i47 }} / {{ i44 }},
           {{ R48 }} := {{ i48 }} / {{ i44 }},
           {{ R49 }} := {{ i49 }} / {{ i44 }})
}
