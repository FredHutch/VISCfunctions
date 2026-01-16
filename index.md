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
- [`two_samp_cont_test()`](https://fredhutch.github.io/VISCfunctions/reference/two_samp_cont_test.md)
  for a t.test (paired or unpaired), Wilcox Rank-Sum, or Wilcox
  Signed-Rank test.
- [`cor_test()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test.md)
  for a Spearman, Pearson, or Kendall correlation test.

Make all pairwise comparisons of a grouping variable (or any categorical
variable), return descriptive statistics and p-values:

- [`pairwise_test_bin()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_bin.md)
  for a Barnard, Fisher’s Exact, Chi-Square or McNemar test.
- [`pairwise_test_cont()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_cont.md)
  for a t.test (paired or unpaired), Wilcox Rank-Sum, or Wilcox
  Signed-Rank test.
- [`cor_test_pairs()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test_pairs.md)
  for a Spearman, Pearson, or Kendall correlation test.

Estimate binomial confidence intervals for a binary vector:

- [`binom_ci()`](https://fredhutch.github.io/VISCfunctions/reference/binom_ci.md)

## Formatting output

- [`paste_tbl_grp()`](https://fredhutch.github.io/VISCfunctions/reference/paste_tbl_grp.md)
  to paste together information (usually descriptive statistics) from
  two groups.
- [`pretty_pvalues()`](https://fredhutch.github.io/VISCfunctions/reference/pretty_pvalues.md)
  to round and format p-values.
- [`stat_paste()`](https://fredhutch.github.io/VISCfunctions/reference/stat_paste.md)
  to combine and format values, such as:
  - Mean (sd)
  - Median \[min, max\]
  - Estimate (SE of Estimate)
  - Estimate (95% CI Lower Bound, Upper Bound)
  - Estimate/Statistic (p value)
- [`escape()`](https://fredhutch.github.io/VISCfunctions/reference/escape.md)
  to protect control characters in a string for use in a latex table or
  caption
- [`collapse_group_row()`](https://fredhutch.github.io/VISCfunctions/reference/collapse_group_row.md)
  as an alternative to
  [`kableExtra::collapse_rows()`](https://rdrr.io/pkg/kableExtra/man/collapse_rows.html)
  for long tables in latex

## Survival and Magnitude Breadth

- [`create_step_curve()`](https://fredhutch.github.io/VISCfunctions/reference/create_step_curve.md)
  to create survival probabilities and generate a risk table
- [`mb_results()`](https://fredhutch.github.io/VISCfunctions/reference/mb_results.md)
  to create step curve info for magnitude breadth (MB) plots and AUC-MB

## Utility functions

- [`round_away_0()`](https://fredhutch.github.io/VISCfunctions/reference/round_away_0.md)
  is an alternative to the
  [`round()`](https://rdrr.io/r/base/Round.html) function to properly
  perform mathematical rounding.
- [`escape()`](https://fredhutch.github.io/VISCfunctions/reference/escape.md)
  is used to inserts a “\\ in front of values, which is needed for
  Latex.
- [`get_full_name()`](https://fredhutch.github.io/VISCfunctions/reference/get_full_name.md)
  looks up a username from Fred Hutch ID.
- [`get_session_info()`](https://fredhutch.github.io/VISCfunctions/reference/get_session_info.md)
  creates a a data frame with session information.

# Vignette

For more information, browse the vignette
(`browseVignettes("VISCfunctions")`).

# Contribute to this package

See our [contibuting
guide](https://fredhutch.github.io/VISCfunctions/CONTRIBUTING.md) to
learn more about how you can contribute to this package.
