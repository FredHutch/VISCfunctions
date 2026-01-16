# Wilson Confidence Interval

**\[superseded\]** `wilson_ci` has been superseded by the use of
[binom_ci](https://fredhutch.github.io/VISCfunctions/reference/binom_ci.md)

## Usage

``` r
wilson_ci(x, conf.level = 0.95)
```

## Arguments

- x:

  vector of type integer (0/1) or logical (TRUE/FALSE)

- conf.level:

  confidence level (between 0 and 1)

## Value

data.frame with with mean (`mean`), and bounds of confidence interval
(`lower`, `upper`)

## Examples

``` r
x <- c(rep(0, 500), rep(1, 500))
wilson_ci(x, conf.level = .90)
#>   mean     lower     upper
#> 1  0.5 0.4740277 0.5259723
```
