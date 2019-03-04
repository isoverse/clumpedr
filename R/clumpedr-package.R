#' A package for clumped isotope analysis in R
#'
#' @author Ilja J. Kocken, \email{i.j.kocken@uu.nl}
#'
#' @details
#' This package incorporates functions to optimize the distribution of
#' (carbonate) standards and the optimal proportion of standards versus the
#' number of sample replicates. In the future, the package will include further
#' scripts for clumped isotope analysis.
#'
#' TODO: include vignettes
#'
#' @section Package options:
#' The functions in this package listen to the \code{genplot} and
#'   \code{quiet} options, which can be set using
#'
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @encoding UTF-8
#' @importFrom stats lm median sd C D cycle na.omit setNames
#' @importFrom rlang enquo quo quos UQ !! !!! := get_expr quo_expr quo_name quo_text quo_is_null quo_is_symbol quo_is_lang is_quosure is_empty is_integerish eval_tidy sym lang_head new_formula f_lhs f_rhs
# #' @importFrom tidyselect everything starts_with ends_with matches vars_select
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread nest unnest extract
#' @importFrom glue glue
#' @importFrom purrr map map_lgl map_chr map_df map_int map_dbl map2 map2_chr map2_lgl map2_dbl map2_int safely is_empty when
#' @importFrom dplyr vars n select select_ rename rename_ arrange desc mutate mutate_ mutate_at mutate_if filter filter_ distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols group_by group_by_ ungroup tally summarize summarise do case_when
# #' @importFrom lubridate interval duration as_datetime
#' @importFrom stringr str_c str_detect str_to_title str_replace str_replace_all str_replace_na str_match str_match_all str_interp str_subset str_extract str_sub fixed
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous scale_y_continuous expand_limits facet_grid facet_wrap labs theme_bw theme %+% is.ggplot aes_ stat_summary geom_smooth geom_hline scale_size_manual scale_alpha_manual scale_colour_manual
NULL

# quiets concerns of R CMD check about . that appears in pipelines
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c(".", "file_id", "mass", "broadid", "masspec", "quiet",
                         "genplot"))
