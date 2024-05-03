# clumpedr 0.3.0 (2024-05-03)

* Fix package install fail: the `default(quiet)` parameter didn't work because I don't export the `default` function.
* Document `quiet` once and inherit the docs everywhere. Explicitly list that it listens to `options(clumpedr.quiet)`.
* Remove all the option setting functions. Users should just use e.g., `option(clumpedr.quiet = TRUE)` if they want a global setting.
* Add `...` argument to most functions, requiring named matches (not just positions) for the more obscure function arguments.
* Remove the `pipe_plot()` function.
* Remove all references to `genplot` and plotting from the main function sources. Closes #17.
* Rebuild most tests so they work and only test one function, if possible.
* Added snapshots for tests that run the code in the vignette (ugly but it works).
* Change `.data$COLUMN` to `"COLUMN"` in all tidyselect functions such as `tidyr::pivot_*`.
* Add GitHub continuous integration.

# clumpedr 0.2.0 (2022-05-11)

* Second release to assign Zenodo DOI.

# clumpedr 0.1.0 (2021-09-14)

* First public release after codebase had somewhat stabilized.
