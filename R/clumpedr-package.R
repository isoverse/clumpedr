#' Clumped isotope analysis in R
#'
#' @author Ilja J. Kocken, \email{i.j.kocken@uu.nl}
#'
#' @details See the vignette `vignette(clumped)` for an elaborate use-case example.
#'
#' @section Package options:
#' The functions in this package listen to the \code{quiet} option.
#'
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @encoding UTF-8
#' @importFrom stats lm median sd C D cycle na.omit setNames approx
#' @importFrom rlang enquo quo quos UQ !! := get_expr quo_squash quo_name quo_text quo_is_null quo_is_symbol is_quosure is_empty is_integerish eval_tidy sym new_formula f_lhs f_rhs .data .env
# #' @importFrom tidyselect everything starts_with ends_with matches vars_select
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyselect all_of
#' @importFrom readr parse_double parse_integer parse_logical
#' @importFrom tidyr pivot_longer pivot_wider nest unnest
#' @importFrom glue glue glue_collapse
#' @importFrom purrr map map_lgl map_chr map_df map_dfr map_int map_dbl map2 map2_chr map2_lgl map2_dbl map2_int safely is_empty when pluck
#' @importFrom broom tidy
#' @importFrom dplyr vars n select select_ rename rename_ arrange desc mutate mutate_ mutate_at mutate_if filter filter_ distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols group_by group_by_ ungroup tally summarize summarise do case_when lead one_of any_of summarise_all first last lead lag pull funs
#' @importFrom stringr str_c str_detect str_to_title str_replace str_replace_all str_replace_na str_match str_match_all str_interp str_subset str_extract str_sub fixed
#' @importFrom utils data getFromNamespace
NULL
# #' @importFrom isoreader iso_turn_info_messages_on iso_turn_info_messages_off iso_get_raw_data iso_get_file_info iso_get_standards_info

# quiets concerns of R CMD check about . that appears in pipelines
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c(".", ".data", "file_id", "mass", "broadid", "masspec", "quiet"))
