initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    clumpedr.quiet = FALSE,
    clumpedr.genplot = TRUE
  )
  options(default_options)
  # set temporary options used during file processing
  # temp_options <- list()
  # options(temp_options)
}

initialize_parameters <- function() {
  # see the function clumpedr.params for references
  default_parameters <- list(
    clumpedr.R13_PDB = 0.01118,
    clumpedr.R18_PDB = 1.008751,
    clumpedr.R17_PDBCO2 = 0.0003931,
    clumpedr.R18_PDBCO2 = 0.00208839,
    clumpedr.lambda = 0.528,
    clumpedr.D47 = 0,
    clumpedr.D48 = 0,
    clumpedr.D49 = 0,
    clumpedr.D17O = 0
  )
  options(default_parameters)
}

.onLoad <- function(libname, pkgname) {
  initialize_options()
  initialize_parameters()
  invisible()
}
