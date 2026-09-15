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

  character. Randomized (de-identified) subject ID.

- group:

  integer. Study group (1 or 2).

- visitno:

  numeric. Visit number (0 = baseline, 1, 2).

- antigen:

  character. Name of the antigen tested.

- magnitude:

  numeric. Response magnitude (MFI-blank, background-subtracted median
  fluorescence intensity).

- response:

  integer. Response call based on MFI-blank (1 = responder, 0 =
  non-responder, NA at baseline).
