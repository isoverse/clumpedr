#' Calculate abundance ratio based on input intensities
#'
#' Calculate abundance ratios by dividing input mass column \eqn{i} over mass
#' 44. \deqn{R_i = mass_i / mass_{44}}
#'
#' @param .data A [tibble][tibble::tibble-package].
#' @param i44 Name of mass 44 column.
#' @param i45 Name of mass 45 column. Same for intensities 46 through 49.
#' @param R45 Desired new name of the calculated ratio for mass 45. Same for
#'   intensities 46 through 49.
#' @export
abundance_ratios <- function(.data,
                             i44 = s44, i45 = s45,
                             i46 = s46, i47 = s47, i48 = s48,
                             i49 = s49,
                             R45 = R45, R46 = R46, R47 = R47,
                             R48 = R48, R49 = R49) {
  i44 <- enquo(i44)
  i45 <- enquo(i45)
  i46 <- enquo(i46)
  i47 <- enquo(i47)
  i48 <- enquo(i48)
  i49 <- enquo(i49)
  R45 <- enquo(R45)
  R46 <- enquo(R46)
  R47 <- enquo(R47)
  R48 <- enquo(R48)
  R49 <- enquo(R49)

  .data %>%
    mutate(!! R45 := !! i45 / !! i44,
           !! R46 := !! i46 / !! i44,
           !! R47 := !! i47 / !! i44,
           !! R48 := !! i48 / !! i44,
           !! R49 := !! i49 / !! i44)
}
