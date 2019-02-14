# clumpedr

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoprocessor)](https://cran.r-project.org/package=clumpedr)
[![Build
Status](https://travis-ci.org/isoverse/clumpedr.svg?branch=master)](https://travis-ci.org/isoverse/clumpedr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/isoverse/clumpedr?branch=master&svg=true)](https://ci.appveyor.com/project/isoverse/clumpedr)

Clumpedr aims to facilitate analysis of clumped isotope data. It builds on
[isoreader](https://github.com/isoverse/isoreader) which is used to import raw data files from the mass spectrometer,
and most of the important data processing parts are based on Huntington et al.
2009 and Daëron et al. 2016.

Clumpedr includes dataprocessing steps such as:

- exclusion of failed cycles, based on a sudden drop of pressure
- simple (probably wrong) background corrections using the mass 47.5 cup
- linear intensity matching of reference gas mass 44 to the sample gas, and
  application of this shift to all other masses
- calculation of delta values
- default calculations use the Brand et al., 2010 pararameters.
- calculation of big delta values
- summary per sample
- per-run correction and calculation of the empirical reference frame

## Installation

clumpedr is currently in development, and thus has not been released on [CRAN](https://CRAN.R-project.org)
yet. Therefore, install it using:

```r
devtools::install_github("isoverse/clumpedr")
```

If this doesn't work, make sure you have `devtools` installed
(`install.packages("devtools")`).

For the development version, use:

```r
devtools::install_github("isoverse/clumpedr", ref = "dev")
```

## Explanation

See the data processing vignette called clumped for an extensive example on how
to use clumpedr!

These will be properly written asap!

```r
vignette("clumped")
```

## Contributing

Contributions to improving the clumped data processing package are more than
welcome! If something doesn't work the way you expect and you don't know how to
fix it, write an issue. If you do know how to fix it, feel free to write a pull
request!

## References

- https://www.isoverse.org/
- https://isoreader.isoverse.org/
- Huntington, K. W., Eiler, J. M., Affek, H. P., Guo, W., Bonifacie, M., Yeung, L. Y., Thiagarajan, N., ..., Methods and limitations of 'clumped' $CO_2$ isotope ($\Delta_{47}$) analysis by gas-source isotope ratio mass spectrometry, Journal of Mass Spectrometry, 44(9), 1318–1329 (2009). http://dx.doi.org/10.1002/jms.1614
- Daëron, M., Blamart, D., Peral, M., & Affek, H. P., Absolute isotopic abundance ratios and the accuracy of $\Delta_{47}$ measurements, Chemical Geology, 442(), 83–96 (2016). http://dx.doi.org/10.1016/j.chemgeo.2016.08.014
