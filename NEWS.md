# VISCfunctions 1.3.0

* Fix bug in `pairwise_test_bin()` and `pairwise_test_cont()` (#111)
* Add option to show library path in packages reproducibility table (#107)
* Maintenance of package and CI (#110, #107)

# VISCfunctions 1.2.4

* Suppress warning from get_full_name() when fragile ldapsearch system call fails on Linux and macOS (#96)

# VISCfunctions 1.2.3

* Use shorter git hash in reproducibility table (#92)
* Update t-test syntax in unit test expectations to avoid new error message introduced in R >= 4.4 (#89)
* Update instructions for installing vignettes with dependencies (#94)
* Update CI (#91)

# VISCfunctions 1.2.2

* Fixed bug in `pairwise_test_cont()` to properly catch paired comparisons where no paired data points exist.
* Updated `get_session_info()` test to account for changes in `sessioninfo::session_info()`
* Removed `car` dependency

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
