# temperature_calculation works

    Code
      temp_test <- temperature_calculation(.data = tibble(D47_final = seq(0.25, 1,
        length.out = 10), slope = 0.333, intercept = 0.22))
    Message
      Info: calculating temperature with slope 0.333 and intercept 0.22, ignoring uncertainty in the calibration.
      If you would like to include temperature uncertainty using bootstrapping, see the package `clumpedcalib` on <https://github.com/japhir/clumpedcalib>

