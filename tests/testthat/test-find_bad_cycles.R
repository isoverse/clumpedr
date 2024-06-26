test_that("find_bad_cycles() works", {
  # simple dataframe that represents all possibilities
  cyc_dat <- tibble::tribble( ~ file_id, ~ type, ~ cycle, ~ v44,
                             # hi is a-ok
                             "hi", "std", 0, 49000,
                             "hi", "std", 1, 39700,
                             "hi", "std", 2, 32000,
                             "hi", "std", 3, 30000,
                             "hi", "std", 4, 29500,
                             "hi", "std", 5, 29400,
                             "hi", "std", 6, 29200,
                             "hi", "smp", 1, 49100,
                             "hi", "smp", 2, 39600,
                             "hi", "smp", 3, 31100,
                             "hi", "smp", 4, 30000,
                             "hi", "smp", 5, 29600,
                             "hi", "smp", 6, 29400,
                             # hu has a starting intensity that's maxed out
                             "hu", "std", 0, 50000,
                             "hu", "std", 1, 50000,
                             "hu", "std", 2, 50000,
                             "hu", "std", 3, 30000,
                             "hu", "std", 4, 29500,
                             "hu", "std", 5, 29400,
                             "hu", "std", 6, 29200,
                             "hu", "smp", 1, 49100,
                             "hu", "smp", 2, 39600,
                             "hu", "smp", 3, 31100,
                             "hu", "smp", 4, 30000,
                             "hu", "smp", 5, 29600,
                             "hu", "smp", 6, 29400,
                             # hey has a a simple drop on smp
                             "hey", "std", 0, 40000,
                             "hey", "std", 1, 39000,
                             "hey", "std", 2, 35000,
                             "hey", "std", 3, 32000,
                             "hey", "std", 4, 31000,
                             "hey", "std", 5, 30000,
                             "hey", "std", 6, 29500,
                             "hey", "smp", 1, 40100,
                             "hey", "smp", 2, 35100,
                             "hey", "smp", 3, 32100,
                             "hey", "smp", 4, 5000, # TODO: while there is a pressure drop, the tapering off isn't detected
                             "hey", "smp", 5, 20,
                             "hey", "smp", 6, 10,
                             # ha has a low cutoff, because the sample intensity was too
                             # high and it couldn't match the ref intensity
                             # TODO: this should be detected by find_init_outliers?
                             "ha", "std", 0, 5000,
                             "ha", "std", 1, 3500,
                             "ha", "std", 2, 2800,
                             "ha", "std", 3, 2000,
                             "ha", "std", 4, 1500,
                             "ha", "std", 5, 1000,
                             "ha", "std", 6, 900,
                             "ha", "smp", 1, 30000,
                             "ha", "smp", 2, 29000,
                             "ha", "smp", 3, 28500,
                             "ha", "smp", 4, 28000,
                             "ha", "smp", 5, 27500,
                             "ha", "smp", 6, 27100,
                             # ho has a rise in pressure
                             "ho", "std", 0, 35000,
                             "ho", "std", 1, 29000,
                             "ho", "std", 2, 25000,
                             "ho", "std", 3, 26000, # TODO: this should be detected
                             "ho", "std", 4, 21000,
                             "ho", "std", 5, 19000,
                             "ho", "std", 6, 18000,
                             "ho", "smp", 1, 35200,
                             "ho", "smp", 2, 29100,
                             "ho", "smp", 3, 25000,
                             "ho", "smp", 4, 22500,
                             "ho", "smp", 5, 21000,
                             "ho", "smp", 6, 20000,
                             )

  bad_cyc <- cyc_dat |>
    mutate(file_id = factor(file_id, levels = unique(cyc_dat$file_id))) |>
    mutate(dis_min = 500, dis_max = 50000, dis_fac = 3) |>
    find_bad_cycles(v44 = "v44", min = "dis_min", max = "dis_max", fac = "dis_fac", relative_to = "init")

  # quick plot for debugging
  ## bad_cyc |>
  ##   ggplot(aes(x = cycle, y = v44, colour = type)) +
  ##   facet_grid(rows = vars(file_id)) +
  ##   geom_line() + geom_point(aes(shape = cycle_drop), size = 5)

  # is it a tibble?
  expect_s3_class(bad_cyc, "tbl_df")
  expect_equal(bad_cyc |> distinct(file_id, cycle_has_drop) |> pull(cycle_has_drop),
               c(## "hi" =
                 FALSE,
                 ## "hu" =
                 TRUE,
                 ## "hey" =
                 TRUE,
                 # in the future we may also want to detect these
                 ## "ha" =
                 FALSE,
                 ## "ho" =
                 FALSE)
            )
  # TODO: create test scenario with regular depletion curve, typical drop,
  # beefy start-slow drop, drop-recover (wobble) and make sure it detects them
  # all correctly
  ## expect_equal(bad_cyc %>%
  ##                filter(file_id == "180814_75_IAM_2_ETH-3.did", cycle == ))
})
