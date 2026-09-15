# Example ICS dataset

A dataset containing response (0/1) and magnitude (continuous) ICS data
for 3 cell populations and 2 antigen stimulations. Can be used to run
VISCfunctions examples.

## Usage

``` r
exampleData_ICS
```

## Format

A data frame with 306 rows and 15 variables:

- pubID:

  character. Randomized (de-identified) subject ID.

- Group:

  character. Study group.

- Visit:

  integer. Visit number (0 = baseline, 1, 2).

- Stim:

  character. Antigen stimulation name (e.g. GAG, POL).

- Parent:

  character. T cell subset used as the parent gate (e.g. CD4/TOTM).

- Population:

  character. Cytokine-expressing cell population (e.g. IFNg, IFNg Or
  IL2).

- Count:

  integer. Number of cytokine-positive cells in the stimulated sample.

- ParentCount:

  integer. Total number of parent T cells in the stimulated sample.

- CountBG:

  integer. Number of cytokine-positive cells in the background
  (unstimulated) sample.

- ParentCountBG:

  integer. Total number of parent T cells in the background sample.

- PercentCell:

  numeric. Percent cytokine-positive cells in the stimulated sample
  (Count / ParentCount).

- PercentCellNet:

  numeric. Net (background-subtracted) percent cytokine-positive cells;
  primary response magnitude measure (PercentCell - background
  PercentCell).

- response_prob:

  numeric. MIMOSA posterior probability of a positive response.

- response_fdr_P:

  numeric. FDR-adjusted p-value from MIMOSA.

- response:

  numeric. Response call based on PercentCellNet (1 = responder, 0 =
  non-responder).
