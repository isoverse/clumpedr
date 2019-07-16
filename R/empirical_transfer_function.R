#' Empirical Transfer Function
#'
#' Calculate and apply the empirical transfer function (ETF). First append the
#' expected values for the standards, then calculate the ETF per Preparation,
#' then apply the ETF to all the samples and standards.
#'
#' @param .data A [tibble][tibble::tibble-package] resulting from [collapse_cycles()].
#' @inheritParams append_expected_values
#' @inheritParams calculate_etf
#' @family empirical transfer functions
# http://r-pkgs.had.co.nz/man.html#dry2
# TODO: use @describeIn or @rdname to also include the references in the wrapper.
#' @export
empirical_transfer_function <- function(.data,
                                        std_names = paste0("ETH-", 1:3),
                                        D47 = c(0.258, 0.256, 0.691), #0.507),
                                        aff = 0.062,
                                        ## outlier = outlier,
                                        raw = D47_raw, exp = expected_D47,
                                        id1 = `Identifier 1`,
                                        session = Preparation,
                                        quiet = default(quiet),
                                        genplot = default(genplot)) {
  # global variables and defaults
  D47_raw <- expected_D47 <- `Identifier 1` <- Preparation <- NULL

  if (!quiet)
    glue("Info: calculating and applying Emperical Transfer Function, by {quo_name(enquo(session))}.") %>%
      message()
  out <- .data %>%
    append_expected_values(std_names = std_names, D47 = D47, aff = aff,
                           id1 = {{ id1 }}, exp = {{ exp }}) %>%
    calculate_etf(raw = {{ raw }}, exp = {{ exp }}, session = {{ session }}, quiet = quiet) %>%
    apply_etf(D47 = {{ raw }})
  if (genplot)
    out %>%
      pipe_plot(plot_etf, std_names = std_names, session = {{ session }})
  out
}
