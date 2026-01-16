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

  randomized subject ID

- Group:

  study group

- Visit:

  visit number: baseline, 1, and 2

- Stim:

  antigen stimulation

- Parent:

  T cell subset

- Population:

  cell population

- Count:

  cell count

- ParentCount:

  parent cell count

- CountBG:

  background cell count

- ParentCountBG:

  parent background cell count

- PercentCell:

  count / parent count

- PercentCellNet:

  response magnitude: percent cell - background percent cell

- response_prob:

  MIMOSA response probability

- response_fdr_P:

  FDR-adjusted p-value

- response:

  response call for percent cell net (0/1)
