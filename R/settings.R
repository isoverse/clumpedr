# retrieve package settings, internal function, not exported
default <- function(name, allow_null = FALSE) {
  name <- enquo(name) %>% quos_to_text(variable = "setting")
  value <- getOption(str_c("clumpedr.", name))
  if (!allow_null && is.null(value)) stop("clumpedr setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("clumpedr.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("clumpedr.", name)))
  return(invisible(value))
}

# retrieve temp option
get_temp <- function(name, allow_null = TRUE) {
  value <- getOption(str_c("clumpedr_temp.", name))
  if (!allow_null && is.null(value)) stop("isoreader temporary setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set temp option
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

#' Turn information messages on/off
#'
#' These functions turn information messages on/off in all subsequent function
#' calls by changing the global settings for the \code{quiet} parameter of most
#' isoreader functions. These functions can be called stand alone or within a
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
