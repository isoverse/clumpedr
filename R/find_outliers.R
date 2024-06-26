#' Find outliers
#'
#' This function finds outliers based on several criteria.
#' Here, we define an outlier as a measurement that has:
#' - an initial mass 44 intensity below `init_low`.
#' - an initial mass 44 intensity above `init_high`.
#' - an imbalance between sample and reference gas mass 44 intensities of more than `diff`.
#' - a clumped value that is more than `nsd_off` standard deviations away from the mean.
#'
#' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
#' @param init_low Minimum initial intensity threshold for mass 44.
#' @param init_high Maximum initial intensity threshold for mass 44.
#' @param init_diff Maximum initial difference in mass 44 threshold between standard and sample gas.
#' @param param49_off Where param_49 > this value is considered an outlier.
#' @param internal_sd The internal standard deviation outlier cutoff.
#' @param n_min Minimum number of aliquots within session to calculate threshold.
#' @param n_id1 Minimum number of aliquots within session to calculate threshold within group.
#' @param nsd_off Number of standard deviations away from the mean threshold.
#' @param D47 The column with \eqn{\Delta_{47}} values.
#' @param session Column name that defines correction session.
#' @param id1 Column name of the sample/standard identifier.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
find_outliers <- function(.data,
                          ...,
                          init_low = 8000, init_high = 40000, init_diff = 1200,
                          param49_off = 1,
                          internal_sd = 0.15,
                          n_min = 5,
                          n_id1 = 5,
                          nsd_off = 4,
                          D47 = D47_raw, #D47_raw_mean,
                          session = Preparation,
                          id1 = `Identifier 1`,
                          quiet = NULL) {
  # default quoted arguments are bad, hmkay
  D47_raw <- Preparation <- `Identifier 1` <- NULL
  rlang::check_dots_empty0(...)
  quiet <- check_quiet(quiet)

  .data %>%
    ## NOTE: cycle outliers are handled elsewhere!
    ## NOTE: init outliers should be computed once the cycles have been collapsed
    ## find_init_outliers(init_low, init_high, init_diff, quiet) %>%
    ## NOTE: param_49 outliers should be computed based on average param_49, once the cycles have been collapsed
    ## find_param49_outliers(param49_off, quiet) %>%
    ## find_R_flags(quiet) %>%
    summarise_outlier(quiet = TRUE) %>% ## this adds the column outlier based on all outlier_ columns
    find_internal_sd_outlier(internal_sd, {{ D47 }}, quiet) %>%
    summarise_outlier(quiet = TRUE) %>%
    find_session_outlier(n = n_min, nsd_off, {{ D47 }}, {{ session }}, quiet) %>%
    summarise_outlier(quiet = TRUE) %>%
    find_session_id1_outlier(n_id1, nsd_off, {{ D47 }}, {{ session }}, {{ id1 }}, quiet) %>%
    ## recalculate the `outlier` column, based on the new outlier reasons
    summarise_outlier(quiet = TRUE) %>%
    as_tibble()
  # TODO: include outlier filtering based on:
  # filter d13C or d18O off
}

##' Find measurements that have wrong initial intensities.
##'
##' Finds values of \code{s44_init} and \code{r44_init} that fall outside of
##' \code{init_low} and \code{init_high}, as well as those where the difference
##' between the reference gas intensity and the sample gass intensity is larger
##' than \code{init_diff}.
##'
##' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
##' @param init_low Column in .data with minimum initial intensity threshold for mass 44.
##' @param init_high Column in .data with maximum initial intensity threshold for mass 44.
##' @param init_diff Column in .data with maximum initial difference in mass 44 threshold between standard and sample gas.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
##' @export
find_init_outliers <- function(.data,
                               init_low, init_high, init_diff,
                               ...,
                               quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  quiet <- check_quiet(quiet)

  if (!quiet) {
    glue("Info: identifying aliquots with {glue_collapse(distinct(.data, {{init_low}}), sep = ', ', last = ' and ')} > i44_init & i44_init < {glue_collapse(distinct(.data, {{init_high}}), sep = ', ', last = ' and ')}, s44 - r44 > {glue_collapse(distinct(.data, {{init_diff}}), sep = ', ', last = ' and ')}.") %>%
      message()
  }

  .data %>%
    mutate(outlier_s44_init_low = .data$s44_init <= ifelse(is.na({{init_low}}), 8000, {{init_low}}),
           outlier_r44_init_low = .data$r44_init <= ifelse(is.na({{init_low}}), 8000, {{init_low}}),
           outlier_s44_init_high = .data$s44_init >= ifelse(is.na({{init_high}}), 40000, {{init_high}}),
           outlier_r44_init_high = .data$r44_init >= ifelse(is.na({{init_high}}), 40000, {{init_high}}),
           outlier_i44_init_diff = abs(.data$s44_init - .data$r44_init) >= ifelse(is.na({{init_diff}}), 3000, {{init_diff}}),
           outlier_init = .data$outlier_s44_init_low | .data$outlier_r44_init_low |
             .data$outlier_s44_init_high | .data$outlier_r44_init_high |
             .data$outlier_i44_init_diff)
}

##' Find param 49 outliers.
##'
##' Find measurements with a param49 value that is greater than \code{param49_off}.
##' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
##' @param param49_off The absolute cutoff value for the parameter 49 value.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
find_param49_outliers <- function(.data, param49_off, ..., quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  quiet <- check_quiet(quiet)

  if (!quiet) {
    glue("Info: identifying rows with `param_49` >= -{param49_off} | <= {param49_off}.") %>%
      message()
  }

  .data %>%
    mutate(outlier_param49 = .data$param_49 >= {{param49_off}} | .data$param_49 <= -{{param49_off}})
}

##' Find internal standard deviation outliers.
##'
##' Outliers are flagged based on an internal standard deviation cutoff value
##' \code{internal_sd}.
##'
##' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
##' @param internal_sd The internal standard deviation cutoff value.
##' @param D47 The column to calculate the internal sd value for.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
##' @export
find_internal_sd_outlier <- function(.data, internal_sd = .15, ...,
                                     D47 = D47_raw,
                                     quiet = NULL) {
  rlang::check_dots_empty0(...)
  `Identifier 1` <- D47_raw <- NULL
  quiet <- check_quiet(quiet)

  if (!quiet) {
    glue("Info: identifying aliquots with internal standard deviation of {quo_name(enquo(D47))} > {internal_sd}.") %>%
      message()
  }
  .data %>%
    mutate(aliquot_sd = sd({{D47}}, na.rm = TRUE),
           outlier_internal_sd = .data$aliquot_sd > .env$internal_sd,
           .by = "file_id")
}

#' Find session outliers.
#'
#' Find outliers that are more than 4 standard deviations away from the session median.
#'
#' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
#' @param n The minimum number of measurements in the session needed to calculate an offset from the median.
#' @param nsd_off The number of standard deviations away from the median.
#' @param D47 The column to calculate the internal sd value for.
#' @param session The session for which to calculate the standard deviation and median values.
#' @param outlier_session Column name of new output column.
#' @export
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
find_session_outlier <- function(.data, ...,
                                 n = 5, nsd_off = 4, D47 = D47_raw, outlier_session = outlier_session_D47,
                                 session = Preparation, quiet = NULL) {
  D47_raw <- Preparation <- outlier_session_D47 <- NULL
  rlang::check_dots_empty0(...)
  quiet <- check_quiet(quiet)

  if (!quiet)
    glue("Info: identifying rows that are >{nsd_off} sd of {quo_name(enquo(D47))} away from the median by {quo_name(enquo(session))}.") %>%
      message()

  .data %>%
    collapse_cycles({{D47}}, id = c(.data$file_id, {{session}}, .data$outlier), outlier = .data$outlier_cycle, funs = list(mean), quiet = TRUE) %>%
    collapse_cycles(.data$mean, id = {{session}}, outlier = .data$outlier, funs = list(~ mean(., na.rm = TRUE),
                                                                     ~ median(., na.rm = TRUE),
                                                                     ~ sd(., na.rm = TRUE),
                                                                     ~ n()), quiet = TRUE) %>%
    unnest(.data$cycle_data) %>%
    mutate({{outlier_session}} := ifelse(.data$n > .env$n, {{D47}} - .data$median > .env$nsd_off * .data$sd, NA))
}

#' Find session outliers by sample/standard type.
#'
#' Find outliers that are more than 4 standard deviations away from the session and sample/standard median.
#'
#' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
#' @param n_id1 The minimum number of measurements of the sample/standard in the session needed to calculate an offset from the median.
#' @param nsd_off The number of standard deviations away from the median.
#' @param D47 The column to calculate the internal sd value for.
#' @param session The session for which to calculate the standard deviation and median values.
#' @param id1 The column that defines the sample/standard name.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
##' @export
find_session_id1_outlier <- function(.data, ...,
                                     n_id1 = 5, nsd_off = 4, D47 = D47_raw,
                                 session = Preparation, id1 = `Identifier 1`, quiet = NULL) {
  D47_raw <- Preparation <- `Identifier 1` <- outlier <- NULL
  rlang::check_dots_empty0(...)
  quiet <- check_quiet(quiet)

  if (!quiet)
    glue("Info: identifying rows that are >{nsd_off} sd of {quo_name(enquo(D47))} away from the median by {quo_name(enquo(session))} and {quo_name(enquo(id1))}.") %>%
      message()

  .data %>%
    group_by({{ session }}, {{ id1 }}) %>%
    mutate(sess_id1_mean = filter(., !outlier) %>% mean({{ D47 }}, na.rm = TRUE),
           sess_id1_med = filter(., !outlier) %>% median({{ D47 }}, na.rm = TRUE),
           sess_id1_sd = filter(., !outlier) %>% sd({{ D47 }}, na.rm = TRUE),
           sess_id1_n = filter(., !outlier) %>% n(),
           outlier_session_id1 = ifelse(.data$sess_id1_n >= n_id1,
                                        abs(.data$sess_id1_med - {{ D47 }}) > nsd_off * .data$sess_id1_sd,
                                        NA)) %>%
    ungroup({{session}}, {{id1}})

}

#' Summarise the outlier columns.
#'
#' Calculate whether a sample is an outlier or not based on all the existing \code{"outlier_"} columns.
#'
#' @param .data A [tibble][tibble::tibble-package] with raw Delta values and file information.
#' @param out_column The name of the outlier column.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quiet
#' @export
summarise_outlier <- function(.data, out_column = outlier, ..., quiet = NULL) {
  rlang::check_dots_empty0(...)
  if (nrow(.data) == 0L) {
    return(tibble(file_id = character()))
  }
  quiet <- check_quiet(quiet)

  if (!quiet) {
    glue("Info: creating a single `outlier` column, based on all \"outlier_\" columns.") %>%
      message()
  }

  outlier <- NULL
  .data %>%
    mutate({{out_column}} := rowSums(select(., tidyselect::starts_with("outlier_")), na.rm = TRUE) > 0)
}

##' Summarize the outlier columns.
##'
##' Calculate whether a sample is an outlier or not based on all the existing \code{"outlier_"} columns.
##'
##' @inheritParams summarise_outlier
##' @export
summarize_outlier <- summarise_outlier
