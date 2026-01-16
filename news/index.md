# Changelog

## VISCfunctions 1.3.1

- Report loaded and attached packages in package reproducibility table
  (#132)
- Report nodename in platform reproducibility table (#121)
- Add B cell example dataset from G001 flow and sequencing file (#108)
- Maintenance of package and CI (#114, \#116, \#124)
- VISCfunctions now depends on R \>= 4.1.0 (#134)
- Auto-generate VISCfunctions pkgdown website with GitHub Actions (#119)

## VISCfunctions 1.3.0

- Fix bug in
  [`pairwise_test_bin()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_bin.md)
  and
  [`pairwise_test_cont()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_cont.md)
  (#111)
- Add option to show library path in packages reproducibility table
  (#107)
- Maintenance of package and CI (#110, \#107)

## VISCfunctions 1.2.4

- Suppress warning from get_full_name() when fragile ldapsearch system
  call fails on Linux and macOS (#96)

## VISCfunctions 1.2.3

- Use shorter git hash in reproducibility table (#92)
- Update t-test syntax in unit test expectations to avoid new error
  message introduced in R \>= 4.4 (#89)
- Update instructions for installing vignettes with dependencies (#94)
- Update CI (#91)

## VISCfunctions 1.2.2

- Fixed bug in
  [`pairwise_test_cont()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_cont.md)
  to properly catch paired comparisons where no paired data points
  exist.
- Updated
  [`get_session_info()`](https://fredhutch.github.io/VISCfunctions/reference/get_session_info.md)
  test to account for changes in
  [`sessioninfo::session_info()`](https://sessioninfo.r-lib.org/reference/session_info.html)
- Removed `car` dependency

## VISCfunctions 1.2.1

- `pairwise_test_cor()` is now named
  [`cor_test_pairs()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test_pairs.md).
  The function
  [`cor_test_pairs()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test_pairs.md)
  now returns a data.frame with information on any ties in each variable
  instead of number of unique values.

## VISCfunctions 1.2.0

- New Functions:
  - [`cor_test()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test.md)
    for testing between two continuous variable
  - [`cor_test_pairs()`](https://fredhutch.github.io/VISCfunctions/reference/cor_test_pairs.md)
    for group pairwise correlation testing (i.e. correlating values
    between all combinations antigens)
  - [`create_step_curve()`](https://fredhutch.github.io/VISCfunctions/reference/create_step_curve.md)
    function for survival curves and other step curves
  - [`mb_results()`](https://fredhutch.github.io/VISCfunctions/reference/mb_results.md)
    for Magnitude Breadth curves and calculating Area under the MB curve
  - [`trapz_sorted()`](https://fredhutch.github.io/VISCfunctions/reference/trapz_sorted.md)
    for Trapezoidal Integration of messy data
  - [`collapse_group_row()`](https://fredhutch.github.io/VISCfunctions/reference/collapse_group_row.md)
    for an alternative to
    [`kableExtra::collapse_rows()`](https://rdrr.io/pkg/kableExtra/man/collapse_rows.html)
    on long tables
  - [`binom_ci()`](https://fredhutch.github.io/VISCfunctions/reference/binom_ci.md)
    to supersede
    [`wilson_ci()`](https://fredhutch.github.io/VISCfunctions/reference/wilson_ci.md)
- Minor Changes:
  - Update README file and add NEWS, CONTRIBUTING, and CONDUCT files
  - Remove [`do()`](https://dplyr.tidyverse.org/reference/do.html)
    examples
  - Add geometric mean/median ability in
    [`pairwise_test_cont()`](https://fredhutch.github.io/VISCfunctions/reference/pairwise_test_cont.md)
  - Updated **Overview** vignette with all additions and changes
- Bug fixes:
  - Fixed issue where
    [`two_samp_bin_test()`](https://fredhutch.github.io/VISCfunctions/reference/two_samp_bin_test.md)
    could give p\>1
  - Fixed issue with reproducibility tables in Mac

## VISCfunctions 1.1.4

- Minor change to remove `devtools` dependency

## VISCfunctions 1.1.3

- New Functions:
  - [`escape()`](https://fredhutch.github.io/VISCfunctions/reference/escape.md)
    to easily make latex friendly columns with `\` in front of special
    characters
  - [`wilson_ci()`](https://fredhutch.github.io/VISCfunctions/reference/wilson_ci.md)
    function for Wilson Confident Intervals
  - `pariwise_test_bin()` for pairwise binary testing comparisons
    (i.e. pairwise group differences in response rates)
- Minor Changes:
  - Adding suffix param to
    [`stat_paste()`](https://fredhutch.github.io/VISCfunctions/reference/stat_paste.md).
    Now easy to add % or something else after each stat, if needed
  - Improved documentation and minor fixes to
    [`two_samp_cont_test()`](https://fredhutch.github.io/VISCfunctions/reference/two_samp_cont_test.md)
  - Added vignette for overview of package

## VISCfunctions 1.1.2

- New Functions:
  - [`get_full_name()`](https://fredhutch.github.io/VISCfunctions/reference/get_full_name.md)
    and
    [`get_session_info()`](https://fredhutch.github.io/VISCfunctions/reference/get_session_info.md)
    for supplemental tables in reports
- Minor Changes:
  - [`round_away_0()`](https://fredhutch.github.io/VISCfunctions/reference/round_away_0.md)
    allows completely NA vectors
  - [`pretty_pvalues()`](https://fredhutch.github.io/VISCfunctions/reference/pretty_pvalues.md)
    now has for “latex”, “pandoc”, “html”, and “no_markup” output
    options
- Bug fixes:
  - [`two_samp_bin_test()`](https://fredhutch.github.io/VISCfunctions/reference/two_samp_bin_test.md):
    Barnard Test now run correctly
  - Change in output type in `pvalue()` from coin package addressed
  - Fix versioning to common three digit method

## VISCfunctions 1.0/1.1

Initial Package Development
