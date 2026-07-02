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

# Installation

The package is available on the Fred Hutch organization GitHub page.

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

# Contribute to this package

See our [contributing guide](CONTRIBUTING.md) to learn more about how
you can contribute to this package.

## Statistical testing and estimates

Compare two groups, return a p-value:

#### Functions Overview

| Function                 | Description                                                                                                                                                                                                               | Dependencies                                                                  | VISC use-case                                                                               | Key Inputs                                                                                       | Key Outputs                                          |
|------------|------------|------------|------------|------------|------------|
| `two_sample_bin_test()`  | Comparisons of a grouping variable for unpaired data                                                                                                                                                                      | Exact::exact.test, stats::fisher.test, stats::chisq.test, stats::mcnemar.test | Barnard, Fishers's Exact, Chi-square, or McNemar tests in PT reports                        | X: vector (2 levels), Y: vector (2 levels), Method: default Barnard                              | p-value for comparing x at the different levels of y |
| `two_sample_cont_test()` | continuous variable compared to binary variable                                                                                                                                                                           | stats::t.test (paired or unpaired), coin::wilcoxsign_test                     | Used within pairwise_test_cont                                                              | X: Numeric vector, Y: vector (2 levels), type of test, paired, hypothesis                        | p-value for comparing x at the different levels of y |
| `cor_test()`             | a wrapper for stats::cor.test, function, except if method = "spearman" is selected and there are ties in at least one variable, in which case this is a wrapper for coin::spearman_test employing the approximate method. | stats::cor.test, coin::spearman_test                                          | Perform a test to determine if the value of the association between two samples equals zero | Two numeric vectors (can include NA values) amongst which correlation test needs to be computed. | P-value from correlation test                        |

Make all pairwise comparisons of a grouping variable (or any categorical
variable), return descriptive statistics and p-values:

| Function               | Description                                                                                                                            | Dependencies                                                          | VISC use-case                                                                                                                              | Key Inputs                                                                                                                                                                            | Key Outputs                                                                        |
|------------|------------|------------|------------|------------|------------|
| `pairwise_test_bin()`  | Make pairwise comparisons of a binary variable                                                                                         | Wilson_ci                                                             | Barnard, Fishers's Exact, Chi-square, or McNemar tests in PT reports                                                                       | X: numeric or logical vector, Group: grouping variable, Method: default Barnard, num_needed_for_test: required sample size per group to perform test, conf_level: confidence interval | Dataframe with columns: Comparison, ResponseStats, ResponseTest, PerfectSeperation |
| `pairwise_test_cont()` | Takes a continuous variable and performs pairwise testing (t-test or wilcox test)                                                      | stats::na.omit, stats::median, <br>stats::sd, two_samp\_<br>cont_test | Group-wise statistical testing on magnitudes, for a t.test (paired or unpaired), Wilcox Rank-Sum, or Wilcox Signed-Rank test in PT reports | X: Numeric vector, Group, type of test, paired, hypothesis, sample size                                                                                                               | Returns a data frame with all possible pairwise comparisons                        |
| `cor_test_pairs()`     | calculates correlation estimate (Spearman, Pearson, or Kendall) and p-value between categorical variable levels (in a pairwise manner) | cor_test()                                                            | calculate the Spearman, Pearson, or Kendall correlation estimate and p-value between the different levels of a categorical variable        | A vector of numeric values (can include NA values), A categorical vector which contains the levels to compare (default Pearson), A vector which contains the id information           | data.frame of summary statistics                                                   |

Estimate binomial confidence intervals for a binary vector:

| Function     | Description                                                | Dependencies         | VISC use-case                                       | Key Inputs                                                    | Key Outputs                     |
|------------|------------|------------|------------|------------|------------|
| `binom_ci()` | Estimate binomial confidence intervals for a binary vector | binom::binom.confint | Calculate response rate 95% CI (replaces wilson_ci) | Vector of ones and zeros (or T/F) and method (default Wilson) | dataframe of summary statistics |

Note that `wilson_ci()` has been superseded by the use of `binom_ci()`.

## Formatting output

| Function       | Description                                                                                                                      | Dependencies | VISC use-case                                                                               | Key Inputs                                  | Key Outputs                                                                       |
|------------|------------|------------|------------|------------|------------|
|                |                                                                                                                                  |              |                                                                                             |                                             |                                                                                   |
|                |                                                                                                                                  |              |                                                                                             |                                             |                                                                                   |
| `stat_paste()` | Takes in values and outputs single, double, or triple statistical summaries                                                      | round_away_0 | mean, mean (sd), median $$25th, 75th$$, median (min, max)                                   | 1, 2, or 3 vectors of numbers or characters | k text outputs, where k = length of the largest input vector                      |
| `escape()`     | Prohibits LaTeX escape characters, when you need to include them in output                                                       | NA           | any output that has a special text character                                                | Character vector                            | Same character vector, with '\\' pasted before escape chars (each vector element) |
|                |                                                                                                                                  |              |                                                                                             |                                             |                                                                                   |
| `round_away_0` | Unlike R round (which rounds towards an even number), this function rounds away from zero and allows retention of trailing zeros | NA           | Generally any time we need to round, additionally when we need to retain significant digits | Numeric vector                              | Numeric or character vector (if trailing_zeros=TRUE)                              |

-   `round_away_0()` is an alternative to the `round()` function to
    properly perform mathematical rounding.

<!-- * `paste_tbl_grp()` to paste together information (usually descriptive statistics) from two groups. -->

<!-- * `stat_paste()` to combine and format values, such as: -->

<!--     + Mean (sd) -->

<!--     + Median [min, max] -->

<!--     + Estimate (SE of Estimate) -->

<!--     + Estimate (95% CI Lower Bound, Upper Bound) -->

<!--     + Estimate/Statistic (p value) -->

<!-- * `escape()` to protect control characters in a string for use in a latex table or caption -->

## Survival and Magnitude Breadth

| Function              | Description                                                                                                     | Dependencies                          | VISC use-case                                         | Key Inputs                                                                                                                                                                                                                                                                                            | Key Outputs                                                                                                                                           |
|------------|------------|------------|------------|------------|------------|
| `create_step_curve()` | Creates data.frame of survival probabilities and "number at risk". Can use to plot step lines (KM or MB curves) | survival::Surv, <br>survival::survfit | Used in mb_results()                                  | x: time values to create x-axis in step curves. For MB "time values" are net-MFI event: event status 0 = censor or 1=event, flip_surv: reverse survival estimates calculated (Default = FALSE). flip_top_x: value to set x for top point for plotting. Only used if flip_surv = TRUE. Default is Inf. | Creates intermediary df (cols: time, surv, n.risk, n.event, n.censor)                                                                                 |
| `mb_results()`        | Creates a data.frame to plot MB curves and AUC                                                                  | create_step_curve, utils::head        | Creates input to create magnitude-breadth (MB) curves | magnitude: values for step curve/AUC response: binary (0/1 or TRUE/FALSE), lower_trunc: lower truncation value (ex: 100), upper_trunc: the upper truncation value (ex: 22,000), x_transform: "log10" (default) or "raw"                                                                               | Creates df for creation of AUC-MB curves (cols: id, magnitude, breadth, n_remaining, n_here, aucMB). Can be individual or combined for grouped input. |
| `trapz_sorted()`      | A wrapper for trapz monotonically sorting values and computing area                                             | pracma::trapz                         |                                                       | x: x-coordinates of points on x-axis, y: y-coordinates of function values, na.rm: boolean for remove NA from x & y or not.                                                                                                                                                                            | integral of function with discrete points (single numeric value)                                                                                      |
