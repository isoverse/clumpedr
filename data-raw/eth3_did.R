eth3 <- isoreader::iso_read_dual_inlet("/home/japhir/Documents/archive/motu/dids/_180814_75/180814_75_IAM_1_ETH-3.did")

several_dids <- isoreader::iso_read_dual_inlet("/home/japhir/Documents/archive/motu/dids/_180814_75/")

standards  <- isoreader::iso_filter_files(several_dids,
                               `Identifier 1` %in% c(paste0("ETH-", 1:4), paste0("IAEA-C", 1:2)))

usethis::use_data(eth3, standards, overwrite=TRUE)
