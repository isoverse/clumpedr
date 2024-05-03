# The full workflow in the vignette works

    Code
      out <- mutate(isoreader::iso_get_raw_data(isoreader::iso_filter_files(standards,
        grepl("Clumped.*met", Method)), include_file_info = c("file_id", "Analysis",
        "Identifier 1")), dis_min = 500, dis_max = 50000, dis_fac = 3)
    Message
      Info: applying file filter, keeping 27 of 27 files
      Info: aggregating raw data from 27 data file(s), including file info 'c("file_id", "Analysis", "Identifier 1")'
    Code
      out <- find_bad_cycles(out, min = "dis_min", max = "dis_max", fac = "dis_fac",
        relative_to = "init")
    Message
      Info: found 0 out of 27 acquisitions with a drop in pressure of mass 44.
    Code
      out <- correct_backgrounds(out, 0.82)
    Message
      Info: adding background based on half-mass with factor 0.82
    Code
      out <- spread_match(out, method = "normal")
    Message
      Info: reshaping data into wide format.
      Info: matching working gas intensities to sample gas, using method normal
    Code
      out <- append_ref_deltas(out, standards)
    Message
      Info: collapsing cycles, calculating sample summaries.
      Info: appending reference gas δ values from 27 data file(s)
    Code
      out <- abundance_ratios(out, i44 = s44, i45 = s45, i46 = s46, i47 = s47, i48 = s48,
        i49 = s49)
    Message
      Info: calculating abundance ratios R[i] = i / 44
    Code
      out <- abundance_ratios(out, i44 = r44, i45 = r45, i46 = r46, i47 = r47, i48 = r48,
        i49 = r49, R45 = R45_wg, R46 = R46_wg, R47 = R47_wg, R48 = R48_wg, R49 = R49_wg)
    Message
      Info: calculating abundance ratios R[i] = i / 44
    Code
      out <- little_deltas(out)
    Message
      Info: calculating δ values with (Ri / Ri_wg - 1) * 1000
    Code
      out <- mutate(out, Mineralogy = "Calcite", R18_PDB = clumpedr:::default(R18_PDB))
      out <- bulk_and_clumping_deltas(out, R18_PDB = unique(out$R18_PDB))
    Message
      Info: calculating δ¹³C, δ¹⁸O, and Δ's.
    Code
      out <- summarise_outlier(out, quiet = TRUE)
      out <- collapse_cycles(out, cols = c(d18O_PDBCO2, d13C_PDB, D47_raw), id = c(
        file_id, Analysis))
    Message
      Info: collapsing cycles, calculating sample summaries.
      defaulting to mean, sd, n, sem, and 95% cl
    Code
      out <- add_info(out, stdinfo, cols = c("file_root", "file_path", "file_subpath",
        "file_datetime", "file_size", "Row", "Peak Center", "Background",
        "Pressadjust", "Reference Refill", "Line", "Sample", "Weight [mg]",
        "Identifier 1", "Comment", "Preparation", "Method", "measurement_info",
        "MS_integration_time.s"))
    Message
      Info: appending measurement information.
    Code
      out <- add_info(out, inits, cols = c("s44_init", "r44_init"))
    Message
      Info: appending measurement information.
    Code
      out <- unnest(out, cols = cycle_data)
      out <- find_init_outliers(out, init_low = 4000, init_high = 40000, init_diff = 3000)
    Message
      Info: identifying aliquots with 4000 > i44_init & i44_init < 40000, s44 - r44 > 3000.
    Code
      out <- empirical_transfer_function(out)
    Message
      Info: calculating and applying Emperical Transfer Function, with D47_raw as a function of expected_D47, for each Preparation.
    Code
      out <- mutate(out, slope = 0.0397, intercept = 0.1518)

