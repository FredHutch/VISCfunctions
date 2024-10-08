
<!-- badges: start -->

[![R build
status](https://github.com/FredHutch/VISCfunctions/workflows/R-CMD-check/badge.svg)](https://github.com/FredHutch/VISCfunctions/actions)
[![Codecov test
coverage](https://codecov.io/gh/FredHutch/VISCfunctions/graph/badge.svg)](https://app.codecov.io/gh/FredHutch/VISCfunctions)
[![License:MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

# VISCfunctions

The package provides functions for common tasks for creating statistical
reports at Vaccine Immunology Statistical Center (VISC). The goal of
VISCfunctions is to:

- provide standard statistical testing and estimating functions
- help format output for PDF and Word reports
- provide frequently used utility functions

for VISC statisticians and programmers at Fred Hutch.

# Installation

The package is available on the Fred Hutch organization GitHub page.

``` r
remotes::install_github("FredHutch/VISCfunctions")

# Use the build_vignettes parameter to access the vignette
# (Note that this may auto-upgrade dependencies - this behavior can be controlled with the `upgrade` argument)
remotes::install_github("FredHutch/VISCfunctions", dependencies = TRUE, build_vignettes = TRUE)
vignette("Overview")
```

# Overview

Below is an overview of the currently available functions in
VISCfunctions.

## Statistical testing and estimates

Compare two groups, return a p-value:

- `two_sample_bin_test()` for a Barnard, Fisher’s Exact, Chi-Square or
  McNemar test.
- `two_samp_cont_test()` for a t.test (paired or unpaired), Wilcox
  Rank-Sum, or Wilcox Signed-Rank test.
- `cor_test()` for a Spearman, Pearson, or Kendall correlation test.

Make all pairwise comparisons of a grouping variable (or any categorical
variable), return descriptive statistics and p-values:

- `pairwise_test_bin()` for a Barnard, Fisher’s Exact, Chi-Square or
  McNemar test.
- `pairwise_test_cont()` for a t.test (paired or unpaired), Wilcox
  Rank-Sum, or Wilcox Signed-Rank test.
- `cor_test_pairs()` for a Spearman, Pearson, or Kendall correlation
  test.

Estimate binomial confidence intervals for a binary vector:

- `binom_ci()`

## Formatting output

- `paste_tbl_grp()` to paste together information (usually descriptive
  statistics) from two groups.
- `pretty_pvalues()` to round and format p-values.
- `stat_paste()` to combine and format values, such as:
  - Mean (sd)
  - Median \[min, max\]
  - Estimate (SE of Estimate)
  - Estimate (95% CI Lower Bound, Upper Bound)
  - Estimate/Statistic (p value)
- `escape()` to protect control characters in a string for use in a
  latex table or caption
- `collapse_group_row()` as an alternative to
  `kableExtra::collapse_rows()` for long tables in latex

## Survival and Magnitude Breadth

- `create_step_curve()` to create survival probabilities and generate a
  risk table
- `mb_results()` to create step curve info for magnitude breadth (MB)
  plots and AUC-MB

## Utility functions

- `round_away_0()` is an alternative to the `round()` function to
  properly perform mathematical rounding.
- `escape()` is used to inserts a “\\” in front of values, which is
  needed for Latex.
- `get_full_name()` looks up a username from Fred Hutch ID.
- `get_session_info()` creates a a data frame with session information.

# Vignette

For more information, browse the vignette
(`browseVignettes("VISCfunctions")`).

# Contribute to this package

See our [contibuting guide](CONTRIBUTING.md) to learn more about how you
can contribute to this package.
