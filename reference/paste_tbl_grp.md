# Pasting Together Information for Two Groups

Paste together information, often statistics, from two groups. There are
two predefined combinations: mean(sd) and median\[min, max\], but user
may also paste any single measure together.

## Usage

``` r
paste_tbl_grp(
  data,
  vars_to_paste = "all",
  first_name = "Group1",
  second_name = "Group2",
  sep_val = " vs. ",
  na_str_out = "---",
  alternative = c("two.sided", "less", "greater"),
  digits = 0,
  trailing_zeros = TRUE,
  keep_all = TRUE,
  verbose = FALSE
)
```

## Arguments

- data:

  input dataset. User must use consistent naming throughout, **with an
  underscore** to separate the group names from the measures (i.e.
  `Group1_mean` and `Group2_mean`). There also must be two columns with
  column names that exactly match the input for `first_name` and
  `second_name` (i.e. 'Group1' and 'Group2'), which are used to form the
  `Comparison` variable.

- vars_to_paste:

  vector of names of common measures to paste together. Can be the
  predefined 'median_min_max' or 'mean_sd', or any variable as long as
  they have matching columns for each group (i.e. Group1_MyMeasure and
  Group2_MyMeasure). Multiple measures can be requested. Default: "all"
  will run 'median_min_max' and 'mean_sd', as well as any pairs of
  columns in the proper format.

- first_name:

  name of first group (string before '\_') . Default is 'Group1'.

- second_name:

  name of second group (string before '\_'). Default is 'Group2'.

- sep_val:

  value to be pasted between the two measures. Default is ' vs. '.

- na_str_out:

  the character to replace missing values with.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "two.sided" (default), "greater" or "less". Will be used to
  determine the character to be pasted between the group names
  (`Comparison` variable). Specifying "two.sided" will use the `sep_val`
  input.

- digits:

  integer indicating the number of decimal places to round to before
  pasting for numeric variables. Default is 0.

- trailing_zeros:

  logical indicating if trailing zeros should be included (i.e. 0.100
  instead of 0.1). Note if set to TRUE output is a character vector.

- keep_all:

  logical indicating if all remaining, unpasted variables in `data`
  should be returned with the pasted variables. Default TRUE.

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed. Default FALSE.

## Value

data.frame with all the pasted values requested. Each name will have
'\_comparison' at the end of the names (i.e. mean_comparison,
median_comparison, ...)

## Details

User must use consistant naming throughout, with a underscore to
separate the group names from the measures (i.e. `Group1_mean` and
`Group2_mean`). There also must be columns defining the group names
(i.e. `Group1` and `Group2`), which are used to form the `Comparison`
variable.

`alternative` included as a parameter so the direction can easily be
seen in one-sided test. If "two.sided" is selected the value to be
pasted between the two group names will be set to `sep_val`, where
"greater" will use " \> " and "less" with use " \< " as the pasting
value.

## Examples

``` r
library(dplyr)
library(tidyr)
data(exampleData_BAMA)

descriptive_stats_by_group <- exampleData_BAMA |>
  group_by(visitno,antigen) |>
  reframe(
    Group1 = unique(group[group == 1]), Group2 = unique(group[group == 2]),
    Group1_n = length(magnitude[group == 1]), Group2_n = length(magnitude[group == 2]),
    Group1_mean = mean(magnitude[group == 1]), Group2_mean = mean(magnitude[group == 2]),
    Group1_sd = sd(magnitude[group == 1]), Group2_sd = sd(magnitude[group == 2]),
    Group1_median = median(magnitude[group == 1]), Group2_median = median(magnitude[group == 2]),
    Group1_min = min(magnitude[group == 1]), Group2_min = min(magnitude[group == 2]),
    Group1_max = max(magnitude[group == 1]), Group2_max = max(magnitude[group == 2])
  )

paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = 'all', first_name = 'Group1',
              second_name = 'Group2', sep_val = " vs. ", digits = 0, keep_all = TRUE)
#>    visitno               antigen Comparison n_comparison mean_comparison
#> 1        0   A1.con.env03 140 CF    1 vs. 2      6 vs. 6       -2 vs. -5
#> 2        0     A244 D11gp120_avi    1 vs. 2      6 vs. 6       75 vs. 84
#> 3        0 B.63521_D11gp120/293F    1 vs. 2      6 vs. 6       66 vs. 12
#> 4        0          B.MN V3 gp70    1 vs. 2      6 vs. 6    -27 vs. -409
#> 5        0    B.con.env03 140 CF    1 vs. 2      6 vs. 6       21 vs. -2
#> 6        0                  gp41    1 vs. 2      6 vs. 6   2017 vs. 3381
#> 7        0                   p24    1 vs. 2      6 vs. 6   9010 vs. 5735
#> 8        1   A1.con.env03 140 CF    1 vs. 2      6 vs. 6    1039 vs. 551
#> 9        1     A244 D11gp120_avi    1 vs. 2      6 vs. 6   5655 vs. 1681
#> 10       1 B.63521_D11gp120/293F    1 vs. 2      6 vs. 6   1720 vs. 1577
#> 11       1          B.MN V3 gp70    1 vs. 2      6 vs. 6     140 vs. 303
#> 12       1    B.con.env03 140 CF    1 vs. 2      6 vs. 6   3150 vs. 5153
#> 13       1                  gp41    1 vs. 2      6 vs. 6 16324 vs. 22559
#> 14       1                   p24    1 vs. 2      6 vs. 6   6745 vs. 3880
#> 15       2   A1.con.env03 140 CF    1 vs. 2      6 vs. 6 15462 vs. 11600
#> 16       2     A244 D11gp120_avi    1 vs. 2      6 vs. 6 29700 vs. 27448
#> 17       2 B.63521_D11gp120/293F    1 vs. 2      6 vs. 6 26839 vs. 26388
#> 18       2          B.MN V3 gp70    1 vs. 2      6 vs. 6  7365 vs. 13936
#> 19       2    B.con.env03 140 CF    1 vs. 2      6 vs. 6 28914 vs. 29123
#> 20       2                  gp41    1 vs. 2      6 vs. 6 10696 vs. 15868
#> 21       2                   p24    1 vs. 2      6 vs. 6  11042 vs. 2717
#>     sd_comparison median_comparison  min_comparison  max_comparison
#> 1       71 vs. 48          3 vs. -8    -123 vs. -75       98 vs. 69
#> 2      31 vs. 152         79 vs. 36      40 vs. -23     112 vs. 385
#> 3       17 vs. 23          73 vs. 8      36 vs. -16       82 vs. 42
#> 4      94 vs. 658       -28 vs. -29  -152 vs. -1504      110 vs. 36
#> 5       61 vs. 67          8 vs. -9     -52 vs. -87     134 vs. 115
#> 6   1519 vs. 3572     1466 vs. 2075     874 vs. 873  4860 vs. 10472
#> 7  10467 vs. 8845     4041 vs. 1450    1880 vs. 832 28818 vs. 23381
#> 8    1210 vs. 348       648 vs. 530      10 vs. 171   3258 vs. 1040
#> 9   7413 vs. 1348     2543 vs. 1046     166 vs. 430  19183 vs. 3704
#> 10  1573 vs. 1894      1563 vs. 676      70 vs. 308   3910 vs. 5116
#> 11   158 vs. 1106        117 vs. 14    -36 vs. -619    329 vs. 2425
#> 12  2365 vs. 4686     2819 vs. 4898     191 vs. 590  5990 vs. 13738
#> 13 8023 vs. 10658   14378 vs. 25038   5381 vs. 8504 26022 vs. 32447
#> 14  4760 vs. 3180     6688 vs. 3266    1728 vs. 964  13559 vs. 9886
#> 15  9629 vs. 5538   15420 vs. 11001   5002 vs. 4682 26700 vs. 20801
#> 16  2184 vs. 4584   29637 vs. 28282 26267 vs. 19268 32274 vs. 31747
#> 17  3533 vs. 6004   26034 vs. 27921 23352 vs. 14839 31956 vs. 31820
#> 18  5782 vs. 9896    6556 vs. 12932   2166 vs. 2752 15885 vs. 25838
#> 19  2163 vs. 3911   28240 vs. 30191 26292 vs. 21395 31628 vs. 32295
#> 20 6993 vs. 10463   10310 vs. 13112   2048 vs. 6414 20089 vs. 29466
#> 21  9775 vs. 1583    10763 vs. 2222   1548 vs. 1102  21449 vs. 4961
#>                        median_min_max_comparison             mean_sd_comparison
#> 1                  3 [-123, 98] vs. -8 [-75, 69]            -2 (71) vs. -5 (48)
#> 2                 79 [40, 112] vs. 36 [-23, 385]           75 (31) vs. 84 (152)
#> 3                    73 [36, 82] vs. 8 [-16, 42]            66 (17) vs. 12 (23)
#> 4            -28 [-152, 110] vs. -29 [-1504, 36]        -27 (94) vs. -409 (658)
#> 5                 8 [-52, 134] vs. -9 [-87, 115]            21 (61) vs. -2 (67)
#> 6         1466 [874, 4860] vs. 2075 [873, 10472]    2017 (1519) vs. 3381 (3572)
#> 7       4041 [1880, 28818] vs. 1450 [832, 23381]   9010 (10467) vs. 5735 (8845)
#> 8             648 [10, 3258] vs. 530 [171, 1040]      1039 (1210) vs. 551 (348)
#> 9         2543 [166, 19183] vs. 1046 [430, 3704]    5655 (7413) vs. 1681 (1348)
#> 10           1563 [70, 3910] vs. 676 [308, 5116]    1720 (1573) vs. 1577 (1894)
#> 11            117 [-36, 329] vs. 14 [-619, 2425]       140 (158) vs. 303 (1106)
#> 12        2819 [191, 5990] vs. 4898 [590, 13738]    3150 (2365) vs. 5153 (4686)
#> 13   14378 [5381, 26022] vs. 25038 [8504, 32447] 16324 (8023) vs. 22559 (10658)
#> 14       6688 [1728, 13559] vs. 3266 [964, 9886]    6745 (4760) vs. 3880 (3180)
#> 15   15420 [5002, 26700] vs. 11001 [4682, 20801]  15462 (9629) vs. 11600 (5538)
#> 16 29637 [26267, 32274] vs. 28282 [19268, 31747]  29700 (2184) vs. 27448 (4584)
#> 17 26034 [23352, 31956] vs. 27921 [14839, 31820]  26839 (3533) vs. 26388 (6004)
#> 18    6556 [2166, 15885] vs. 12932 [2752, 25838]   7365 (5782) vs. 13936 (9896)
#> 19 28240 [26292, 31628] vs. 30191 [21395, 32295]  28914 (2163) vs. 29123 (3911)
#> 20   10310 [2048, 20089] vs. 13112 [6414, 29466] 10696 (6993) vs. 15868 (10463)
#> 21     10763 [1548, 21449] vs. 2222 [1102, 4961]   11042 (9775) vs. 2717 (1583)

paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = c("mean", "median_min_max"),
              alternative= "less", keep_all = FALSE)
#>    Comparison mean_comparison                     median_min_max_comparison
#> 1       1 < 2       -2 vs. -5                 3 [-123, 98] vs. -8 [-75, 69]
#> 2       1 < 2       75 vs. 84                79 [40, 112] vs. 36 [-23, 385]
#> 3       1 < 2       66 vs. 12                   73 [36, 82] vs. 8 [-16, 42]
#> 4       1 < 2    -27 vs. -409           -28 [-152, 110] vs. -29 [-1504, 36]
#> 5       1 < 2       21 vs. -2                8 [-52, 134] vs. -9 [-87, 115]
#> 6       1 < 2   2017 vs. 3381        1466 [874, 4860] vs. 2075 [873, 10472]
#> 7       1 < 2   9010 vs. 5735      4041 [1880, 28818] vs. 1450 [832, 23381]
#> 8       1 < 2    1039 vs. 551            648 [10, 3258] vs. 530 [171, 1040]
#> 9       1 < 2   5655 vs. 1681        2543 [166, 19183] vs. 1046 [430, 3704]
#> 10      1 < 2   1720 vs. 1577           1563 [70, 3910] vs. 676 [308, 5116]
#> 11      1 < 2     140 vs. 303            117 [-36, 329] vs. 14 [-619, 2425]
#> 12      1 < 2   3150 vs. 5153        2819 [191, 5990] vs. 4898 [590, 13738]
#> 13      1 < 2 16324 vs. 22559   14378 [5381, 26022] vs. 25038 [8504, 32447]
#> 14      1 < 2   6745 vs. 3880       6688 [1728, 13559] vs. 3266 [964, 9886]
#> 15      1 < 2 15462 vs. 11600   15420 [5002, 26700] vs. 11001 [4682, 20801]
#> 16      1 < 2 29700 vs. 27448 29637 [26267, 32274] vs. 28282 [19268, 31747]
#> 17      1 < 2 26839 vs. 26388 26034 [23352, 31956] vs. 27921 [14839, 31820]
#> 18      1 < 2  7365 vs. 13936    6556 [2166, 15885] vs. 12932 [2752, 25838]
#> 19      1 < 2 28914 vs. 29123 28240 [26292, 31628] vs. 30191 [21395, 32295]
#> 20      1 < 2 10696 vs. 15868   10310 [2048, 20089] vs. 13112 [6414, 29466]
#> 21      1 < 2  11042 vs. 2717     10763 [1548, 21449] vs. 2222 [1102, 4961]

paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = 'all', first_name = 'Group1',
              second_name = 'Group2', sep_val = " vs. ",
              alternative = 'less', digits = 5, keep_all = FALSE)
#>    Comparison n_comparison             mean_comparison
#> 1       1 < 2      6 vs. 6       -1.79167 vs. -5.16667
#> 2       1 < 2      6 vs. 6       74.95833 vs. 83.58333
#> 3       1 < 2      6 vs. 6       65.79167 vs. 12.08333
#> 4       1 < 2      6 vs. 6    -26.75000 vs. -409.16667
#> 5       1 < 2      6 vs. 6       21.25000 vs. -2.12500
#> 6       1 < 2      6 vs. 6   2017.41667 vs. 3380.62500
#> 7       1 < 2      6 vs. 6   9010.16667 vs. 5735.33333
#> 8       1 < 2      6 vs. 6    1038.58333 vs. 550.91667
#> 9       1 < 2      6 vs. 6   5655.16667 vs. 1680.70833
#> 10      1 < 2      6 vs. 6   1719.87500 vs. 1577.16667
#> 11      1 < 2      6 vs. 6     140.04167 vs. 302.70833
#> 12      1 < 2      6 vs. 6   3149.50000 vs. 5152.87500
#> 13      1 < 2      6 vs. 6 16323.66667 vs. 22558.66667
#> 14      1 < 2      6 vs. 6   6745.00000 vs. 3880.45833
#> 15      1 < 2      6 vs. 6 15462.20833 vs. 11599.91667
#> 16      1 < 2      6 vs. 6 29699.79167 vs. 27448.16667
#> 17      1 < 2      6 vs. 6 26839.12500 vs. 26387.54167
#> 18      1 < 2      6 vs. 6  7364.91667 vs. 13935.75000
#> 19      1 < 2      6 vs. 6 28914.29167 vs. 29123.45833
#> 20      1 < 2      6 vs. 6 10695.62500 vs. 15868.20833
#> 21      1 < 2      6 vs. 6  11041.70833 vs. 2716.79167
#>                 sd_comparison           median_comparison
#> 1       70.52401 vs. 48.24486        2.62500 vs. -8.12500
#> 2      31.40717 vs. 151.55376       78.87500 vs. 36.50000
#> 3       17.06855 vs. 22.77151        73.00000 vs. 7.75000
#> 4      93.98191 vs. 657.60959     -28.00000 vs. -29.12500
#> 5       61.48069 vs. 67.36148        7.50000 vs. -8.87500
#> 6   1519.19892 vs. 3572.37247   1466.00000 vs. 2075.00000
#> 7  10466.69376 vs. 8845.37576   4041.12500 vs. 1449.62500
#> 8    1209.99794 vs. 347.74372     648.00000 vs. 529.75000
#> 9   7413.16625 vs. 1348.19803   2542.87500 vs. 1046.12500
#> 10  1572.95930 vs. 1894.47811    1562.87500 vs. 675.87500
#> 11   158.30488 vs. 1105.90485      117.37500 vs. 14.12500
#> 12  2364.74946 vs. 4685.93171   2819.25000 vs. 4897.62500
#> 13 8023.22147 vs. 10658.00931 14377.75000 vs. 25038.50000
#> 14  4760.00364 vs. 3180.21395   6688.12500 vs. 3265.50000
#> 15  9628.93196 vs. 5537.86394 15419.75000 vs. 11000.62500
#> 16  2184.43202 vs. 4583.82720 29636.75000 vs. 28282.00000
#> 17  3532.87984 vs. 6003.76938 26033.62500 vs. 27920.87500
#> 18  5782.41369 vs. 9896.06080  6555.87500 vs. 12931.50000
#> 19  2162.59904 vs. 3910.76717 28240.00000 vs. 30191.12500
#> 20 6992.66463 vs. 10462.80168 10309.87500 vs. 13111.75000
#> 21  9775.01390 vs. 1583.32778  10762.87500 vs. 2222.50000
#>                 min_comparison              max_comparison
#> 1     -122.75000 vs. -74.75000       98.00000 vs. 68.75000
#> 2       39.50000 vs. -23.25000     112.00000 vs. 385.00000
#> 3       35.75000 vs. -16.00000       81.75000 vs. 41.50000
#> 4   -152.25000 vs. -1504.50000      110.50000 vs. 36.25000
#> 5      -52.50000 vs. -87.25000     133.50000 vs. 115.00000
#> 6      873.75000 vs. 873.25000  4860.50000 vs. 10472.50000
#> 7     1879.50000 vs. 831.50000 28817.50000 vs. 23381.00000
#> 8        9.75000 vs. 171.00000   3258.50000 vs. 1040.25000
#> 9      166.25000 vs. 430.00000  19183.25000 vs. 3704.00000
#> 10      69.50000 vs. 307.75000   3910.50000 vs. 5115.50000
#> 11    -36.25000 vs. -618.75000    329.00000 vs. 2425.00000
#> 12     191.25000 vs. 589.75000  5989.75000 vs. 13738.25000
#> 13   5380.75000 vs. 8504.50000 26021.75000 vs. 32447.00000
#> 14    1728.00000 vs. 963.50000  13559.00000 vs. 9886.25000
#> 15   5002.25000 vs. 4681.75000 26699.75000 vs. 20800.75000
#> 16 26267.00000 vs. 19268.50000 32273.50000 vs. 31747.25000
#> 17 23351.75000 vs. 14839.25000 31956.00000 vs. 31820.00000
#> 18   2166.25000 vs. 2752.50000 15885.25000 vs. 25838.25000
#> 19 26292.50000 vs. 21395.25000 31627.50000 vs. 32295.00000
#> 20   2047.75000 vs. 6414.25000 20088.75000 vs. 29466.50000
#> 21   1548.25000 vs. 1101.75000  21448.75000 vs. 4961.00000
#>                                                            median_min_max_comparison
#> 1                  2.62500 [-122.75000, 98.00000] vs. -8.12500 [-74.75000, 68.75000]
#> 2                 78.87500 [39.50000, 112.00000] vs. 36.50000 [-23.25000, 385.00000]
#> 3                    73.00000 [35.75000, 81.75000] vs. 7.75000 [-16.00000, 41.50000]
#> 4            -28.00000 [-152.25000, 110.50000] vs. -29.12500 [-1504.50000, 36.25000]
#> 5                 7.50000 [-52.50000, 133.50000] vs. -8.87500 [-87.25000, 115.00000]
#> 6         1466.00000 [873.75000, 4860.50000] vs. 2075.00000 [873.25000, 10472.50000]
#> 7       4041.12500 [1879.50000, 28817.50000] vs. 1449.62500 [831.50000, 23381.00000]
#> 8              648.00000 [9.75000, 3258.50000] vs. 529.75000 [171.00000, 1040.25000]
#> 9         2542.87500 [166.25000, 19183.25000] vs. 1046.12500 [430.00000, 3704.00000]
#> 10           1562.87500 [69.50000, 3910.50000] vs. 675.87500 [307.75000, 5115.50000]
#> 11            117.37500 [-36.25000, 329.00000] vs. 14.12500 [-618.75000, 2425.00000]
#> 12        2819.25000 [191.25000, 5989.75000] vs. 4897.62500 [589.75000, 13738.25000]
#> 13   14377.75000 [5380.75000, 26021.75000] vs. 25038.50000 [8504.50000, 32447.00000]
#> 14       6688.12500 [1728.00000, 13559.00000] vs. 3265.50000 [963.50000, 9886.25000]
#> 15   15419.75000 [5002.25000, 26699.75000] vs. 11000.62500 [4681.75000, 20800.75000]
#> 16 29636.75000 [26267.00000, 32273.50000] vs. 28282.00000 [19268.50000, 31747.25000]
#> 17 26033.62500 [23351.75000, 31956.00000] vs. 27920.87500 [14839.25000, 31820.00000]
#> 18    6555.87500 [2166.25000, 15885.25000] vs. 12931.50000 [2752.50000, 25838.25000]
#> 19 28240.00000 [26292.50000, 31627.50000] vs. 30191.12500 [21395.25000, 32295.00000]
#> 20   10309.87500 [2047.75000, 20088.75000] vs. 13111.75000 [6414.25000, 29466.50000]
#> 21     10762.87500 [1548.25000, 21448.75000] vs. 2222.50000 [1101.75000, 4961.00000]
#>                                        mean_sd_comparison
#> 1             -1.79167 (70.52401) vs. -5.16667 (48.24486)
#> 2            74.95833 (31.40717) vs. 83.58333 (151.55376)
#> 3             65.79167 (17.06855) vs. 12.08333 (22.77151)
#> 4         -26.75000 (93.98191) vs. -409.16667 (657.60959)
#> 5             21.25000 (61.48069) vs. -2.12500 (67.36148)
#> 6     2017.41667 (1519.19892) vs. 3380.62500 (3572.37247)
#> 7    9010.16667 (10466.69376) vs. 5735.33333 (8845.37576)
#> 8       1038.58333 (1209.99794) vs. 550.91667 (347.74372)
#> 9     5655.16667 (7413.16625) vs. 1680.70833 (1348.19803)
#> 10    1719.87500 (1572.95930) vs. 1577.16667 (1894.47811)
#> 11       140.04167 (158.30488) vs. 302.70833 (1105.90485)
#> 12    3149.50000 (2364.74946) vs. 5152.87500 (4685.93171)
#> 13 16323.66667 (8023.22147) vs. 22558.66667 (10658.00931)
#> 14    6745.00000 (4760.00364) vs. 3880.45833 (3180.21395)
#> 15  15462.20833 (9628.93196) vs. 11599.91667 (5537.86394)
#> 16  29699.79167 (2184.43202) vs. 27448.16667 (4583.82720)
#> 17  26839.12500 (3532.87984) vs. 26387.54167 (6003.76938)
#> 18   7364.91667 (5782.41369) vs. 13935.75000 (9896.06080)
#> 19  28914.29167 (2162.59904) vs. 29123.45833 (3910.76717)
#> 20 10695.62500 (6992.66463) vs. 15868.20833 (10462.80168)
#> 21   11041.70833 (9775.01390) vs. 2716.79167 (1583.32778)


# Same example wit tidyverse in single pipe


exampleData_BAMA |>
 mutate(group = paste0("Group", group)) |>
 group_by(group, visitno, antigen) |>
 reframe(N = n(), mean = mean(magnitude), sd = sd(magnitude),
         median = median(magnitude), min = min(magnitude),
         max = max(magnitude), q95_fun = quantile(magnitude, 0.95)) |>
 pivot_longer(-(group:antigen)) |> # these three chains create a wide dataset
 unite(temp, group, name) |>
 pivot_wider(names_from = temp, values_from = value) |>
 mutate(Group1 = "Group 1", Group2 = "Group 2") |>
 paste_tbl_grp()
#>    visitno               antigen          Comparison N_comparison
#> 1        0   A1.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 2        0     A244 D11gp120_avi Group 1 vs. Group 2      6 vs. 6
#> 3        0 B.63521_D11gp120/293F Group 1 vs. Group 2      6 vs. 6
#> 4        0          B.MN V3 gp70 Group 1 vs. Group 2      6 vs. 6
#> 5        0    B.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 6        0                  gp41 Group 1 vs. Group 2      6 vs. 6
#> 7        0                   p24 Group 1 vs. Group 2      6 vs. 6
#> 8        1   A1.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 9        1     A244 D11gp120_avi Group 1 vs. Group 2      6 vs. 6
#> 10       1 B.63521_D11gp120/293F Group 1 vs. Group 2      6 vs. 6
#> 11       1          B.MN V3 gp70 Group 1 vs. Group 2      6 vs. 6
#> 12       1    B.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 13       1                  gp41 Group 1 vs. Group 2      6 vs. 6
#> 14       1                   p24 Group 1 vs. Group 2      6 vs. 6
#> 15       2   A1.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 16       2     A244 D11gp120_avi Group 1 vs. Group 2      6 vs. 6
#> 17       2 B.63521_D11gp120/293F Group 1 vs. Group 2      6 vs. 6
#> 18       2          B.MN V3 gp70 Group 1 vs. Group 2      6 vs. 6
#> 19       2    B.con.env03 140 CF Group 1 vs. Group 2      6 vs. 6
#> 20       2                  gp41 Group 1 vs. Group 2      6 vs. 6
#> 21       2                   p24 Group 1 vs. Group 2      6 vs. 6
#>    mean_comparison  sd_comparison median_comparison  min_comparison
#> 1        -2 vs. -5      71 vs. 48          3 vs. -8    -123 vs. -75
#> 2        75 vs. 84     31 vs. 152         79 vs. 36      40 vs. -23
#> 3        66 vs. 12      17 vs. 23          73 vs. 8      36 vs. -16
#> 4     -27 vs. -409     94 vs. 658       -28 vs. -29  -152 vs. -1504
#> 5        21 vs. -2      61 vs. 67          8 vs. -9     -52 vs. -87
#> 6    2017 vs. 3381  1519 vs. 3572     1466 vs. 2075     874 vs. 873
#> 7    9010 vs. 5735 10467 vs. 8845     4041 vs. 1450    1880 vs. 832
#> 8     1039 vs. 551   1210 vs. 348       648 vs. 530      10 vs. 171
#> 9    5655 vs. 1681  7413 vs. 1348     2543 vs. 1046     166 vs. 430
#> 10   1720 vs. 1577  1573 vs. 1894      1563 vs. 676      70 vs. 308
#> 11     140 vs. 303   158 vs. 1106        117 vs. 14    -36 vs. -619
#> 12   3150 vs. 5153  2365 vs. 4686     2819 vs. 4898     191 vs. 590
#> 13 16324 vs. 22559 8023 vs. 10658   14378 vs. 25038   5381 vs. 8504
#> 14   6745 vs. 3880  4760 vs. 3180     6688 vs. 3266    1728 vs. 964
#> 15 15462 vs. 11600  9629 vs. 5538   15420 vs. 11001   5002 vs. 4682
#> 16 29700 vs. 27448  2184 vs. 4584   29637 vs. 28282 26267 vs. 19268
#> 17 26839 vs. 26388  3533 vs. 6004   26034 vs. 27921 23352 vs. 14839
#> 18  7365 vs. 13936  5782 vs. 9896    6556 vs. 12932   2166 vs. 2752
#> 19 28914 vs. 29123  2163 vs. 3911   28240 vs. 30191 26292 vs. 21395
#> 20 10696 vs. 15868 6993 vs. 10463   10310 vs. 13112   2048 vs. 6414
#> 21  11042 vs. 2717  9775 vs. 1583    10763 vs. 2222   1548 vs. 1102
#>     max_comparison q95_fun_comparison
#> 1        98 vs. 69          76 vs. 57
#> 2      112 vs. 385        109 vs. 307
#> 3        82 vs. 42          80 vs. 40
#> 4       110 vs. 36          89 vs. 32
#> 5      134 vs. 115         108 vs. 90
#> 6   4860 vs. 10472      4279 vs. 8669
#> 7  28818 vs. 23381    24778 vs. 19017
#> 8    3258 vs. 1040       2802 vs. 989
#> 9   19183 vs. 3704     16618 vs. 3538
#> 10   3910 vs. 5116      3649 vs. 4420
#> 11    329 vs. 2425       322 vs. 1927
#> 12  5990 vs. 13738     5948 vs. 11682
#> 13 26022 vs. 32447    25861 vs. 32234
#> 14  13559 vs. 9886     12531 vs. 8440
#> 15 26700 vs. 20801    26055 vs. 19145
#> 16 32274 vs. 31747    32146 vs. 31556
#> 17 31956 vs. 31820    31428 vs. 31230
#> 18 15885 vs. 25838    14619 vs. 25611
#> 19 31628 vs. 32295    31589 vs. 32016
#> 20 20089 vs. 29466    19322 vs. 28684
#> 21  21449 vs. 4961     21080 vs. 4811
#>                        median_min_max_comparison             mean_sd_comparison
#> 1                  3 [-123, 98] vs. -8 [-75, 69]            -2 (71) vs. -5 (48)
#> 2                 79 [40, 112] vs. 36 [-23, 385]           75 (31) vs. 84 (152)
#> 3                    73 [36, 82] vs. 8 [-16, 42]            66 (17) vs. 12 (23)
#> 4            -28 [-152, 110] vs. -29 [-1504, 36]        -27 (94) vs. -409 (658)
#> 5                 8 [-52, 134] vs. -9 [-87, 115]            21 (61) vs. -2 (67)
#> 6         1466 [874, 4860] vs. 2075 [873, 10472]    2017 (1519) vs. 3381 (3572)
#> 7       4041 [1880, 28818] vs. 1450 [832, 23381]   9010 (10467) vs. 5735 (8845)
#> 8             648 [10, 3258] vs. 530 [171, 1040]      1039 (1210) vs. 551 (348)
#> 9         2543 [166, 19183] vs. 1046 [430, 3704]    5655 (7413) vs. 1681 (1348)
#> 10           1563 [70, 3910] vs. 676 [308, 5116]    1720 (1573) vs. 1577 (1894)
#> 11            117 [-36, 329] vs. 14 [-619, 2425]       140 (158) vs. 303 (1106)
#> 12        2819 [191, 5990] vs. 4898 [590, 13738]    3150 (2365) vs. 5153 (4686)
#> 13   14378 [5381, 26022] vs. 25038 [8504, 32447] 16324 (8023) vs. 22559 (10658)
#> 14       6688 [1728, 13559] vs. 3266 [964, 9886]    6745 (4760) vs. 3880 (3180)
#> 15   15420 [5002, 26700] vs. 11001 [4682, 20801]  15462 (9629) vs. 11600 (5538)
#> 16 29637 [26267, 32274] vs. 28282 [19268, 31747]  29700 (2184) vs. 27448 (4584)
#> 17 26034 [23352, 31956] vs. 27921 [14839, 31820]  26839 (3533) vs. 26388 (6004)
#> 18    6556 [2166, 15885] vs. 12932 [2752, 25838]   7365 (5782) vs. 13936 (9896)
#> 19 28240 [26292, 31628] vs. 30191 [21395, 32295]  28914 (2163) vs. 29123 (3911)
#> 20   10310 [2048, 20089] vs. 13112 [6414, 29466] 10696 (6993) vs. 15868 (10463)
#> 21     10763 [1548, 21449] vs. 2222 [1102, 4961]   11042 (9775) vs. 2717 (1583)
```
