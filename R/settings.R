# Taken from isoreader/isoprocessor
# https://github.com/isoverse/isoprocessor/R/settings.R
# #' @export
## isoreader::iso_turn_info_messages_on

# #' @export
## isoreader::iso_turn_info_messages_off

# retrieve package settings
default <- function(name, allow_null = FALSE) {
  name_exp <- rlang::enexpr(name)
  if (rlang::is_symbol(name_exp))
    name <- rlang::as_name(name_exp)
  else if (is.character(name_exp))
    name <- name_exp
  else
    stop("don't know how to process setting expression '", rlang::as_label(name_exp), "'", call. = FALSE)
  value <- getOption(str_c("clumpedr.", name))
  if (!allow_null && is.null(value)) stop("clumpedr setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

#' set package setting
#'
#' Essentially the same function as in isoreader except with the isoprocessor. prefix
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("clumpedr.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("clumpedr.", name)))
  return(invisible(value))
}

get_temp <- function(name, allow_null = TRUE) {
  value <- getOption(str_c("clumpedr_temp.", name))
  if (!allow_null && is.null(value)) stop("clumpedr temporary setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

#' Set temporary option
#'
#' Set a temporary option for parallel processing in clumpedr.
#'
#' @param name name of the temporary option
#' @param value value of the temporary option
#' @export
set_temp <- function(name, value) {
  options(list(value) %>% setNames(str_c("clumpedr_temp.", name)))
  return(invisible(value))
}

# helper function to transfer option settings between processors
get_all_options <- function(with_temp = FALSE) {
  all_opts <- options()
  pattern <- if(with_temp) "^clumpedr(_temp)?\\." else "^clumpedr\\."
  all_opts[str_detect(names(all_opts), pattern)]
}

#' Get the current default parameters
#'
#' Retrieve a table with all default function parameters for this package. To
#' set read parameters, see
#' TODO: add clumpedr_set_default_parameters()
#' To set messaging and caching
#' parameters see
#' and see. For a piping
#' compatible version of this function, see
#' TODO: create show_default_reader_parameters function
#' @family settings functions
#' @export
clumpedr_get_default_parameters <- function() {
  c("quiet", "genplot", "R13_PDB", "R18_PDB", "R17_PDBCO2", "R18_PDBCO2",
    "lambda", "D47", "D48", "D49", "D18O") %>%
    sapply(function(x) list(default(!!x))) %>%
    {
      data_frame(parameter = names(.),
                 value = as.character(unlist(.)))
    }
}

#' Show the current default parameters
#'
#' Shows a table with the default function parameters for this package.
#' @param data a data frame - returned invisibly as is if provided (e.g. in the
#'   middle of a pipeline)
#' @param func function to use for formatting the reader parameters table, e.g.
#'   \code{\link[knitr]{kable}}. Note that if the output is in RMarkdown
#'   chunks, the chunk option must have \code{results="asis"} for the table to
#'   be correctly formatted.
#' @param ... additional parameters to forward to the \code{func} function
#' @family settings functions
#' @export
clumpedr_show_default_parameters <- function(data = NULL, func = NULL, ...) {
  if (!default("quiet")) message("Info: clumpedr package current default parameters")

  if (!is.null(func))
    print(do.call(func, args = c(list(x = clumpedr_get_default_parameters()), list(...))))
  else
    print(clumpedr_get_default_parameters())

  # for pipeline
  return(invisible(data))
}

#' Turn information messages on/off
#'
#' These functions turn information messages on/off in all subsequent function
#' calls by changing the global settings for the \code{quiet} parameter of most
#' clumpedr functions. These functions can be called stand alone or within a
#' pipeline to turn messages on/off at a certain point during the pipeline.
#'
#' @name clumpedr_info_messages
NULL

#' @rdname clumpedr_info_messages
#' @family settings functions
#' @export
clumpedr_turn_info_messages_on <- function(data = NULL) {
  set_default("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(invisible(data))
}

#' @rdname clumpedr_info_messages
#' @export
clumpedr_turn_info_messages_off <- function(data = NULL) {
  set_default("quiet", TRUE)
  if (!missing(data)) return(invisible(data))
}

# update quiet returns update function for on.exit
update_quiet <- function(quiet) {
  if (quiet != default(quiet)) {
    quiet_setting <- default(quiet)
    set_default("quiet", quiet)
    return(function() set_default("quiet", quiet_setting))
  } else {
    return(function() {})
  }
}

# TODO: add functions to turn genplot on/of.
