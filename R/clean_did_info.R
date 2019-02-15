#' Clean up the did file info
#'
#' Converts mass spectrometer metadata into the correct types and appends the
#' initial intensity of mass 44.
#'
#' @param did An iso file, resulting from [isoreader::iso_read_dual_inlet()].
#' @param ... Additional options to pass to [isoreader::iso_get_file_info()].
#' @inheritParams parse_info
#' @seealso [isoreader::iso_read_dual_inlet()]
#' @seealso [isoreader::iso_get_file_info()]
#' @family metadata cleaning functions
#' @export
clean_did_info  <- function(did, ..., masspec = NULL, std_names = paste0("ETH-", 1:4),
                            oth_name = "other", quiet = default(quiet)) {
    if (!quiet) {
        message("Info: appending and parsing file info")
        ## TODO: include parameters  with std_names = {stringr::str_c(std_names)} and oth_name = {oth_name}
    }
    parsedinfo <- did %>%
        iso_get_file_info(...) %>%
        parse_info(masspec, std_names, oth_name)

    inits <- get_inits(did)

    full_join(parsedinfo, inits, by = "file_id")
}

#' Parse info into appropriate types.
#'
#' Converts the information from [isoreader::iso_get_file_info()] into the
#' appropriate types.
#'
#' @param dat The result of [isoreader::iso_get_file_info()].
#' @param masspec The name of the mass spectrometer to append.
#' @param std_names Character vector of the standard names to find in
#'     'Identifier 1'.
#' @param oth_name Single general name to assign to non-standard "other" measurements.
#' @family metadata cleaning functions
parse_info <- function(dat, masspec, std_names = paste0("ETH-", 1:4), oth_name = "other") {
    dat %>%
        mutate(
            Row = parse_integer(Row),
            `Peak Center` = parse_logical(`Peak Center`),
            Background = parse_double(Background),
            Pressadjust = parse_logical(Pressadjust),
            `Reference Refill` = parse_logical(`Reference Refill`),
            Line = parse_integer(Line),
            Sample = parse_integer(Sample),
            `Weight [mg]` = parse_double(`Weight [mg]`),
            Analysis = parse_integer(Analysis),
            Preparation = parse_integer(Preparation),
            ## append new column infos
            masspec = ifelse(!is.null(masspec), masspec, ""),
            broadid = ifelse(`Identifier 1` %in% std_names,
                             `Identifier 1`, oth_name))
}

#' Add initial intensities
#' @family metadata cleaning functions
get_inits <- function(did) {
    did %>%
        isoreader::iso_get_raw_data(quiet = TRUE) %>%
        filter(type == "standard" & cycle == 0 | type == "sample" & cycle == 1) %>%
        ## TODO: check if v44.mV is present
        select(file_id, type, v44.mV) %>%
        spread(type, v44.mV) %>%
        select(file_id, s44_init = sample, r44_init = standard)
}
