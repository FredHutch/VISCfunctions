# Example BAMA dataset

A dataset containing response (0/1) and magnitude (continuous) BAMA data
for 7 antigens, 2 groups, and 3 visits. Can be used to run VISCfunctions
examples.

## Usage

``` r
exampleData_BAMA
```

## Format

A data frame with 252 rows and 6 variables:

- pubID:

  randomized subject ID

- group:

  study group

- visitno:

  visit number: baseline, 1, and 2

- antigen:

  antigen tested

- magnitude:

  magnitude of response, continuous, MFI\*

- response:

  response call for MFI\* (0/1, NA at baseline)
