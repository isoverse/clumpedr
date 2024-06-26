test_that("The full workflow in the vignette works", {
stdinfo <- isoreader::iso_get_file_info(standards)

inits <- get_inits(
  isoreader::iso_get_raw_data(standards,
                              include_file_info = "Analysis"))
expect_snapshot({
  out <- standards |>
    isoreader::iso_filter_files(grepl("Clumped.*met", Method)) |>
    isoreader::iso_get_raw_data(include_file_info = c("file_id", "Analysis", "Identifier 1")) |>
    mutate(dis_min = 500, dis_max = 50000, dis_fac = 3)
  out <- out |>
    find_bad_cycles(min = "dis_min", max = "dis_max",
                    fac = "dis_fac", relative_to = "init")
  out <- out |>
    correct_backgrounds(0.82)
  out <- out |>
    spread_match(method = "normal")
  out <- out |>
    append_ref_deltas(standards)
  out <- out |>
    abundance_ratios(i44 = s44, i45 = s45, i46 = s46, i47 = s47, i48 = s48, i49 = s49)
  out <- out |>
    abundance_ratios(i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48, i49 = r49,
                     R45 = R45_wg, R46 = R46_wg, R47 = R47_wg, R48 = R48_wg, R49 = R49_wg)
  out <- out |>
    little_deltas()
  out <- out |>
    mutate(Mineralogy = "Calcite", R18_PDB = clumpedr:::default(R18_PDB))
  out <- out |>
    bulk_and_clumping_deltas( R18_PDB = unique(out$R18_PDB))
    # outlier on the cycle level now contains all the reasons for cycle outliers
  out <- out |>
    summarise_outlier(quiet = TRUE)
  out <- out |>
    collapse_cycles(cols = c(d18O_PDBCO2, d13C_PDB, D47_raw), id = c(file_id, Analysis))
  out <- out |>
    add_info(stdinfo,
             cols = c("file_root", "file_path", "file_subpath",
                      "file_datetime", "file_size", "Row",
                      "Peak Center", "Background", "Pressadjust",
                      "Reference Refill", "Line", "Sample",
                      "Weight [mg]", "Identifier 1", "Comment",
                      "Preparation", "Method", "measurement_info",
                      "MS_integration_time.s"))
  out <- out |>
    add_info(inits, cols = c("s44_init", "r44_init"))
  out <- out |>
    unnest(cols = cycle_data)
  out <- out |>
    find_init_outliers(init_low = 4000, init_high = 40000, init_diff = 3000)
  out <- out |>
    empirical_transfer_function()
  out <- out |>
    mutate(slope = 0.0397, intercept = 0.1518)
  # TODO
  ## out <- out |>
  ##   temperature_calculation(D47 = D47_etf)
  })
})
