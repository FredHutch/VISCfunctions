---
editor_options: 
  markdown: 
    wrap: 72
---

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

| Function                 | Description                                                                                                                                                                                                               | Dependencies                                                                  | VISC use-case                                                                               | Key Inputs                                                                                       | Key Outputs                                          |
|------------|------------|------------|------------|------------|------------|
| `two_sample_bin_test()`  | Comparisons of a grouping variable for unpaired data                                                                                                                                                                      | Exact::exact.test, stats::fisher.test, stats::chisq.test, stats::mcnemar.test | Barnard, Fishers's Exact, Chi-square, or McNemar tests in PT reports                        | X: vector (2 levels), Y: vector (2 levels), Method: default Barnard                              | p-value for comparing x at the different levels of y |
| `two_sample_cont_test()` | continuous variable compared to binary variable                                                                                                                                                                           | stats::t.test (paired or unpaired), coin::wilcoxsign_test                     | Used within pairwise_test_cont                                                              | X: Numeric vector, Y: vector (2 levels), type of test, paired, hypothesis                        | p-value for comparing x at the different levels of y |
| `cor_test()`             | a wrapper for stats::cor.test, function, except if method = "spearman" is selected and there are ties in at least one variable, in which case this is a wrapper for coin::spearman_test employing the approximate method. | stats::cor.test, coin::spearman_test                                          | Perform a test to determine if the value of the association between two samples equals zero | Two numeric vectors (can include NA values) amongst which correlation test needs to be computed. | P-value from correlation test                        |
