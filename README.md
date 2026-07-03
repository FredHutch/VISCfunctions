
<!-- badges: start -->

[![R build
status](https://github.com/FredHutch/VISCfunctions/workflows/R-CMD-check/badge.svg)](https://github.com/FredHutch/VISCfunctions/actions)
[![Codecov test
coverage](https://codecov.io/gh/FredHutch/VISCfunctions/graph/badge.svg)](https://app.codecov.io/gh/FredHutch/VISCfunctions)
[![License:MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

# VISCfunctions

VISCfunctions is an open-source R package that helps automate and
standardize common tasks that are part of the process of analysis at the
Vaccine & Immunology Statistical Center (VISC) at Fred Hutch. The scope
of VISCfunctions is to aid VISC statisticians and programmers by
providing:

-   standard statistical testing and estimating functions
-   standard figure and table formatting functions
-   utility functions for formatting PDF and Word output
-   utility functions that aid with transparency and reproducibility
-   example datasets for common lab assays

## Installation

The package is available through the Fred Hutch GitHub organization:

``` r
remotes::install_github("FredHutch/VISCfunctions")

# Use the build_vignettes parameter to access the vignette
# (Note that this may auto-upgrade dependencies - this behavior can be controlled with the `upgrade` argument)
remotes::install_github("FredHutch/VISCfunctions", dependencies = TRUE, build_vignettes = TRUE)
```

Documentation can be viewed at
<https://fredhutch.github.io/VISCfunctions> and also through standard R
syntax such as:

``` r
library(VISCfunctions)
browseVignettes("VISCfunctions")
vignette("Overview")
help(two_samp_bin_test)  # or ?two_sample_bin_test; similarly for other functions
```

## Contribute to this package

You can file an issue or feature request on GitHub:
<https://github.com/FredHutch/VISCfunctions/issues>

See our [contributing guide](CONTRIBUTING.md) to learn more about how
you can contribute.
