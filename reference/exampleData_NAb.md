# Example NAb dataset

A dataset containing response (0/1) and magnitude (continuous) NAb data
for 6 isolates, 4 groups, and 1 visit. Can be used to run VISCfunctions
examples.

## Usage

``` r
exampleData_NAb
```

## Format

A data frame with 210 rows and 9 variables:

- pubID:

  randomized subject ID

- group:

  study group

- visitno:

  visit number

- celltype:

  cell type

- isolate:

  isolate tested

- titer_mod_50:

  truncated response magnitude value, ID50 titer (min. 10)

- titer_mod_80:

  truncated response magnitude value, ID80 titer (min. 10)

- response_50:

  response call for ID50 titer (0/1)

- response_80:

  response call for ID80 titer (0/1)
