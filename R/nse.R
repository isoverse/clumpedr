# Convert quo to text accounting for plain text and symbol quos
# stolen from isoreader!

# use the function quos_to_text from isoreader, which isn't exported
quos_to_text <- utils::getFromNamespace("quos_to_text", "isoreader")

## previous version just copied it.
# Convert quo to text accounting for plain text and symbol quos
## quos_to_text <- function(lquos, check_for_validity = TRUE, variable = "variable") {
##   single_quo <- is_quosure(lquos)
##   lquos <- if(single_quo) quos(!!lquos) else quos(!!!lquos)
##   are_text_quos <- map_lgl(lquos, ~is.character(quo_squash(.x)))
##   are_symbol_quos <- map_lgl(lquos, quo_is_symbol)
##   # check for validity
##   if (check_for_validity && !all(ok <- are_text_quos | are_symbol_quos)) {
##     params <-
##       str_c(names(lquos)[!ok] %>% { ifelse(nchar(.) > 0, str_c(., " = "), .) },
##             map_chr(lquos[!ok], quo_text)) %>%
##       collapse("', '", last = "' and '")
##     if (sum(!ok) > 1)
##       glue("parameters '{params}' do not refer to valid {variable} names") %>% stop(call. = FALSE)
##     else
##       glue("parameter '{params}' does not refer to a valid {variable} name") %>% stop(call. = FALSE)
##   }

##   text_quos <-
##     map2_chr(lquos, are_text_quos, function(lquo, is_text)
##       if(is_text) quo_squash(lquo) else quo_text(lquo)) %>%
##     as.list()
##   if (single_quo) return(text_quos[[1]])
##   else return(text_quos)
## }
