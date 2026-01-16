# Rounding Using Round Away From 0 Method

round_away_0 takes a numeric vector, rounds them to a specified digit
amount using the round away from 0 method for ties (i.e. 1.5). This is
the SAS method for rounding.

## Usage

``` r
round_away_0(x, digits = 0, trailing_zeros = FALSE)
```

## Arguments

- x:

  numeric vector (can include NA values).

- digits:

  positive integer of length 1 between 0 (default) and 14, giving the
  amount of digits to round to.

- trailing_zeros:

  logical indicating if trailing zeros should included (i.e. 0.100
  instead of 0.1). Note is set to TRUE output is a character vector

## Value

if `trailing_zeros = TRUE` returns a character vector of rounded values
with trailing zeros, otherwise returns a numeric vector of rounded
values.

## Details

`round_away_0` is not designed for use at precision levels \<= 1e-15

## Examples

``` r
vals_to_round = c(NA,-3.5:3.5,NA)
# [1]   NA -3.5 -2.5 -1.5 -0.5  0.5  1.5  2.5  3.5   NA

# round() will round to even numbers when tied at X.5
round(vals_to_round)
#>  [1] NA -4 -2 -2  0  0  2  2  4 NA
# [1] NA -4 -2 -2  0  0  2  2  4 NA

# round_away_0() will round away from 0 when tied at X.5
round_away_0(vals_to_round)
#>  [1] NA -4 -3 -2 -1  1  2  3  4 NA
# [1] NA -4 -3 -2 -1  1  2  3  4 NA

# Can force trailing zeros (will output character vector)
round_away_0(vals_to_round, digits = 2, trailing_zeros = TRUE)
#>  [1] NA      "-3.50" "-2.50" "-1.50" "-0.50" "0.50"  "1.50"  "2.50"  "3.50" 
#> [10] NA     
```
