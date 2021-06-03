# VISCfunctions 1.2.1

* `pairwise_test_cor()` is now named `cor_test_pairs()`. The function `cor_test_pairs()` now returns a data.frame with information on any ties in each variable instead of number of unique values.


# VISCfunctions 1.2.0

* New Functions:
  * `cor_test()` for testing between two continuous variable 
  * `cor_test_pairs()` for group pairwise correlation testing (i.e. correlating values between all combinations antigens)
  * `create_step_curve()` function for survival curves and other step curves 
  * `mb_results()` for Magnitude Breadth curves and calculating Area under the MB curve
  * `trapz_sorted()` for Trapezoidal Integration of messy data
  * `collapse_group_row()` for an alternative to `kableExtra::collapse_rows()` on long tables
  * `binom_ci()` to supersede `wilson_ci()`
* Minor Changes:
  * Update README file and add NEWS, CONTRIBUTING, and CONDUCT files
  * Remove `do()` examples
  * Add geometric mean/median ability in `pairwise_test_cont()`
  * Updated **Overview** vignette with all additions and changes
* Bug fixes:
  * Fixed issue where `two_samp_bin_test()` could give p>1 
  * Fixed issue with reproducibility tables in Mac


# VISCfunctions 1.1.4

* Minor change to remove `devtools` dependency


# VISCfunctions 1.1.3

* New Functions:
  * `escape()` to easily make latex friendly columns with `\` in front of special characters
  * `wilson_ci()` function for Wilson Confident Intervals
  * `pariwise_test_bin()` for pairwise binary testing comparisons (i.e. pairwise group differences in response rates)
* Minor Changes:
  * Adding suffix param to `stat_paste()`. Now easy to add % or something else after each stat, if needed
  * Improved documentation and minor fixes to `two_samp_cont_test()`
  * Added vignette for overview of package
  
  
# VISCfunctions 1.1.2

* New Functions:
  * `get_full_name()` and `get_session_info()` for supplemental tables in reports
* Minor Changes:
  * `round_away_0()` allows completely NA vectors
  * `pretty_pvalues()` now has for "latex", "pandoc", "html", and "no_markup" output options
* Bug fixes:
  * `two_samp_bin_test()`: Barnard Test now run correctly
  * Change in output type in `pvalue()` from coin package addressed
  * Fix versioning to common three digit method 


# VISCfunctions 1.0/1.1

Initial Package Development
