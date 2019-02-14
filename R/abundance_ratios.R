#' Calculate abundance ratio based on input intensities
#'
#' @param dat Input dataframe.
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
#' @export
abundance_ratios <- function(dat,
                             i44 = quo(s44), i45 = quo(s45),
                             i46 = quo(s46), i47 = quo(s47), i48 = quo(s48),
                             i49 = quo(s49),
                             R45 = quo(R45), R46 = quo(R46), R47 = quo(R47),
                             R48 = quo(R48), R49 = quo(R49)) {
    dat %>%
        mutate(!!R45 := !!i45 / !!i44,
               !!R46 := !!i46 / !!i44,
               !!R47 := !!i47 / !!i44,
               !!R48 := !!i48 / !!i44,
               !!R49 := !!i49 / !!i44)
}
