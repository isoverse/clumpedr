eth3 <- isoreader::iso_read_dual_inlet("/run/user/1000/gvfs/smb-share:server=geofile02.geo.uu.nl,share=geo-labs/@RawData/253pluskiel/Raw Data/Kiel Raw Data/_180814_75/180814_75_IAM_1_ETH-3.did")

several_dids <- isoreader::iso_read_dual_inlet("/run/user/1000/gvfs/smb-share:server=geofile02.geo.uu.nl,share=geo-labs/@RawData/253pluskiel/Raw Data/Kiel Raw Data/_180814_75/")

standards  <-
  several_dids %>%
  iso_filter_files(`Identifier 1` %in% c(paste0("ETH-", 1:4), paste0("IAEA-C", 1:2)))

usethis::use_data(eth3, standards, overwrite=TRUE)
