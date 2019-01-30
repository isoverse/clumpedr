#' Calculate abundance ratio based on input intensities
#'
#' @param dat Input dataframe.
#' @param i44 Name of mass 44 column. Same for i45--i49.
#' @param R45 Desired new name of the calculated ratio for mass 45. Same for
#'     R46--R49.
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
