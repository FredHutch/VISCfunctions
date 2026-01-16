# Rounds and combines up to three numbers into table friendly presentation

Takes in up to 3 numeric values, rounds each to a specified digit amount
(if numeric), and then combines them accordingly.

## Usage

``` r
stat_paste(
  stat1,
  stat2 = NULL,
  stat3 = NULL,
  digits = 0,
  trailing_zeros = TRUE,
  bound_char = c("(", "[", "{", "|"),
  sep = ", ",
  na_str_out = "---",
  suffix = NULL
)
```

## Arguments

- stat1:

  first statistic to be pasted.

- stat2:

  second statistic to be pasted (optional).

- stat3:

  third statistic to be pasted (optional).

- digits:

  positive integer of length 1 between 0 (default) and 14, giving the
  amount of digits to round to.

- trailing_zeros:

  logical indicating if trailing zeros should included (i.e. 0.100
  instead of 0.1). Note is set to TRUE output is a character vector

- bound_char:

  the character to be used between stat1 and stat2/stat3. Available
  options are '(' (default), '\[', '{', and '\|'.

- sep:

  the string to be used between stat2 and stat3. The default is ', '.

- na_str_out:

  the character to replace missing values with.

- suffix:

  a character string to add at the end of each stat (i.e. `%` if doing
  response rates)

## Value

string of combined values

## Details

One value provided - returns a rounded value or the missing character.
Two values - returns stat1 (stat2). e.g., mean (sd) Three values -
returns stat1 (stat2, stat3). e.g., estimate (95% lower, 95% upper) or
median \[min, max\]

Currently the method does work with string variables, but of course
rounding would not be relevant for strings.

## Examples

``` r
stat_paste(5.109293)
#> [1] "5"
stat_paste(NA)
#> [1] "---"
stat_paste(5.109293, 2.145)
#> [1] "5 (2)"
stat_paste(5.109293, 1.7645, 8.0345)
#> [1] "5 (2, 8)"
stat_paste(NA, NA, NA)
#> [1] "---"
stat_paste(5.109, "p < 0.001", digits = 3)
#> [1] "5.109 (p < 0.001)"
stat_paste(c(rep(5,5),NA),c(1:5,NA),c(1,NA,2,NA,3,NA),bound_char = '[')
#> [1] "5 [1, 1]"   "5 [2, ---]" "5 [3, 2]"   "5 [4, ---]" "5 [5, 3]"  
#> [6] "---"       

library(dplyr)
exampleData_BAMA |>
group_by(antigen, visitno, group) |>
summarise(median_min_max = stat_paste(median(magnitude, na.rm = TRUE),
                                        min(magnitude, na.rm = TRUE),
                                        max(magnitude, na.rm = TRUE)),
          .groups = "keep")
#> # A tibble: 42 × 4
#> # Groups:   antigen, visitno, group [42]
#>    antigen             visitno group median_min_max     
#>    <chr>                 <dbl> <int> <chr>              
#>  1 A1.con.env03 140 CF       0     1 3 (-123, 98)       
#>  2 A1.con.env03 140 CF       0     2 -8 (-75, 69)       
#>  3 A1.con.env03 140 CF       1     1 648 (10, 3258)     
#>  4 A1.con.env03 140 CF       1     2 530 (171, 1040)    
#>  5 A1.con.env03 140 CF       2     1 15420 (5002, 26700)
#>  6 A1.con.env03 140 CF       2     2 11001 (4682, 20801)
#>  7 A244 D11gp120_avi         0     1 79 (40, 112)       
#>  8 A244 D11gp120_avi         0     2 36 (-23, 385)      
#>  9 A244 D11gp120_avi         1     1 2543 (166, 19183)  
#> 10 A244 D11gp120_avi         1     2 1046 (430, 3704)   
#> # ℹ 32 more rows
```
