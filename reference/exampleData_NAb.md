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

  character. Randomized (de-identified) subject ID.

- group:

  numeric. Study group (2, 3, 4, or 5).

- visitno:

  integer. Visit number.

- celltype:

  character. Cell line used in the neutralization assay (e.g. TZM-bl).

- isolate:

  character. Name of the HIV isolate (pseudovirus) tested.

- titer_mod_50:

  numeric. ID50 neutralization titer, truncated at a minimum of 10
  (below-detection values set to 10).

- titer_mod_80:

  numeric. ID80 neutralization titer, truncated at a minimum of 10
  (below-detection values set to 10).

- response_50:

  numeric. Response call based on ID50 titer (1 = responder, 0 =
  non-responder).

- response_80:

  numeric. Response call based on ID80 titer (1 = responder, 0 =
  non-responder).
