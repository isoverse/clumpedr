# Taken from isoreader/isoprocessor
# https://github.com/isoverse/isoprocessor/R/settings.R

# retrieve package settings, internal function, not exported
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
  c("quiet", ## "genplot",
    "R13_PDB", "R18_PDB", "R17_PDBCO2", "R18_PDBCO2",
    "lambda", "D47", "D48", "D49"## , "D18O"
    ) %>%
    sapply(function(x) list(default(!!x))) %>%
    {
      tibble(parameter = names(.),
             value = as.character(unlist(.)))
    }
}
