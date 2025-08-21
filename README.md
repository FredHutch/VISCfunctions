
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

#### Functions Overview

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `two_sample_bin_test()` | Comparisons of a grouping variable for unpaired data | Exact::exact.test, stats::fisher.test, stats::chisq.test, stats::mcnemar.test | Barnard, Fishers’s Exact, Chi-square, or McNemar tests in PT reports | X: vector (2 levels), Y: vector (2 levels), Method: default Barnard | p-value for comparing x at the different levels of y |
| `two_sample_cont_test()` | continuous variable compared to binary variable | stats::t.test (paired or unpaired), coin::wilcoxsign_test | Used within pairwise_test_cont | X: Numeric vector, Y: vector (2 levels), type of test, paired, hypothesis | p-value for comparing x at the different levels of y |
| `cor_test()` | a wrapper for stats::cor.test, function, except if method = “spearman” is selected and there are ties in at least one variable, in which case this is a wrapper for coin::spearman_test employing the approximate method. | stats::cor.test, coin::spearman_test | Perform a test to determine if the value of the association between two samples equals zero | Two numeric vectors (can include NA values) amongst which correlation test needs to be computed. | P-value from correlation test |

Make all pairwise comparisons of a grouping variable (or any categorical
variable), return descriptive statistics and p-values:

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `pairwise_test_bin()` | Make pairwise comparisons of a binary variable | Wilson_ci | Barnard, Fishers’s Exact, Chi-square, or McNemar tests in PT reports | X: numeric or logical vector, Group: grouping variable, Method: default Barnard, num_needed_for_test: required sample size per group to perform test, conf_level: confidence interval | Dataframe with columns: Comparison, ResponseStats, ResponseTest, PerfectSeperation |
| `pairwise_test_cont()` | Takes a continuous variable and performs pairwise testing (t-test or wilcox test) | stats::na.omit, stats::median, <br>stats::sd, two_samp\_<br>cont_test | Group-wise statistical testing on magnitudes, for a t.test (paired or unpaired), Wilcox Rank-Sum, or Wilcox Signed-Rank test in PT reports | X: Numeric vector, Group, type of test, paired, hypothesis, sample size | Returns a data frame with all possible pairwise comparisons |
| `cor_test_pairs()` | calculates correlation estimate (Spearman, Pearson, or Kendall) and p-value between categorical variable levels (in a pairwise manner) | cor_test() | calculate the Spearman, Pearson, or Kendall correlation estimate and p-value between the different levels of a categorical variable | A vector of numeric values (can include NA values), A categorical vector which contains the levels to compare (default Pearson), A vector which contains the id information | data.frame of summary statistics |

Estimate binomial confidence intervals for a binary vector:

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `binom_ci()` | Estimate binomial confidence intervals for a binary vector | binom::binom.confint | Calculate response rate 95% CI (replaces wilson_ci) | Vector of ones and zeros (or T/F) and method (default Wilson) | dataframe of summary statistics |

## Formatting output

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `paste_tbl_grp()` | Paste together information (statistical summaries) from two groups | round_away_0, stat_paste | Output tables with group comparison summaries | (group) Wide formatted dataframe | Long dataframe based on combining summaries for two groups |
| `pretty_pvalues()` | Takes a vector of p-values, rounds them to a specified digit amount, allows options for emphasizing p-values \< the defined significance level, and returns a character for missing | round_away_0, kableExtra::<br>cell_spec() | Presenting p values in table outputs, with highlights for significant values | Numeric vector | Character vector of pretty p values |
| `stat_paste()` | Takes in values and outputs single, double, or triple statistical summaries | round_away_0 | mean, mean (sd), median \[25th, 75th\], median (min, max) | 1, 2, or 3 vectors of numbers or characters | k text outputs, where k = length of the largest input vector |
| `escape()` | Prohibits LaTeX escape characters, when you need to include them in output | NA | any output that has a special text character | Character vector | Same character vector, with ‘\\’ pasted before escape chars (each vector element) |
| `collapse_group_row()` | replacing repeated rows in a data.frame into NA for nice printing | NA | Remove repeats by group / visit in a table dataset before output | Dataframe | Same dataframe, with repeated values set to NA |
| `round_away_0` | Unlike R round (which rounds towards an even number), this function rounds away from zero and allows retention of trailing zeros | NA | Generally any time we need to round, additionally when we need to retain significant digits | Numeric vector | Numeric or character vector (if trailing_zeros=TRUE) |

<!-- * `paste_tbl_grp()` to paste together information (usually descriptive statistics) from two groups. -->
<!-- * `pretty_pvalues()` to round and format p-values. -->
<!-- * `stat_paste()` to combine and format values, such as: -->
<!--     + Mean (sd) -->
<!--     + Median [min, max] -->
<!--     + Estimate (SE of Estimate) -->
<!--     + Estimate (95% CI Lower Bound, Upper Bound) -->
<!--     + Estimate/Statistic (p value) -->
<!-- * `escape()` to protect control characters in a string for use in a latex table or caption -->
<!-- * `collapse_group_row()` as an alternative to `kableExtra::collapse_rows()` for long tables in latex -->

## Survival and Magnitude Breadth

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `create_step_curve()` | Creates data.frame of survival probabilities and “number at risk”. Can use to plot step lines (KM or MB curves) | survival::Surv, <br>survival::survfit | Used in mb_results() | x: time values to create x-axis in step curves. For MB “time values” are net-MFI event: event status 0 = censor or 1=event, flip_surv: reverse survival estimates calculated (Default = FALSE). flip_top_x: value to set x for top point for plotting. Only used if flip_surv = TRUE. Default is Inf. | Creates intermediary df (cols: time, surv, n.risk, n.event, n.censor) |
| `mb_results()` | Creates a data.frame to plot MB curves and AUC | create_step_curve, utils::head | Creates input to create magnitude-breadth (MB) curves | magnitude: values for step curve/AUC response: binary (0/1 or TRUE/FALSE), lower_trunc: lower truncation value (ex: 100), upper_trunc: the upper truncation value (ex: 22,000), x_transform: “log10” (default) or “raw” | Creates df for creation of AUC-MB curves (cols: id, magnitude, breadth, n_remaining, n_here, aucMB). Can be individual or combined for grouped input. |
| `trapz_sorted()` | A wrapper for trapz monotonically sorting values and computing area | pracma::trapz |  | x: x-coordinates of points on x-axis, y: y-coordinates of function values, na.rm: boolean for remove NA from x & y or not. | integral of function with discrete points (single numeric value) |

<!-- * `create_step_curve()` to create survival probabilities and generate a risk table -->
<!-- * `mb_results()` to create step curve info for magnitude breadth (MB) plots and AUC-MB -->

## Utility functions

| Function | Description | Dependencies | VISC use-case | Key Inputs | Key Outputs |
|----|----|----|----|----|----|
| `get_full_name()` | looks up a username from Fred Hutch ID |  |  | ID to look full name up. If null (default) looks up ID of current user | character vector |
| `get_session_info()` | creates a a data frame with session information | my_session_info | Creating tables used at the end of reports, for reproducibility. Most of the information is based off of sessioninfo::session_info() |  | dataframe |

<!-- * `round_away_0()` is an alternative to the `round()` function to properly perform mathematical rounding. -->
<!-- * `escape()` is used to inserts a "\\" in front of values, which is needed for Latex. -->
<!-- * `get_full_name()` looks up a username from Fred Hutch ID. -->
<!-- * `get_session_info()` creates a a data frame with session information. -->

# Vignette

For more information, browse the vignette
(`browseVignettes("VISCfunctions")`).

# Contribute to this package

See our [contibuting guide](CONTRIBUTING.md) to learn more about how you
can contribute to this package.
