# Example B-cell flow and sequencing dataset from G001

A subset of the B-cell flow cytometry and BCR sequencing summary file
from the G001 clinical trial (germline-targeting HIV vaccine). Contains
PBMC samples only.

## Usage

``` r
G001_Bcell_flow_seq_PBMC
```

## Format

A data frame with 3995 rows and 22 variables:

- pubid:

  character. Randomized (de-identified) participant ID.

- group:

  integer. Study group (1 = low dose, 2 = high dose; NA for placebo).

- treatment:

  character. Treatment description (e.g. "20 µg eOD-GT8 60mer + AS01B",
  "DPBS sucrose").

- dose:

  numeric. Vaccine dose in micrograms (NA for placebo).

- dose_unit:

  character. Unit for the dose field (µg); NA for placebo.

- visitno:

  character. Visit identifier as a zero-padded string (e.g. "02", "06").

- visit:

  integer. Visit time point in weeks relative to first vaccination
  (negative = pre-vaccination).

- visit_units:

  character. Units for the visit column (always "weeks").

- sample_type:

  character. Biological sample type (always "PBMC").

- probeset:

  character. Label for the probe/stain panel used (e.g. "G001 PBMC (KO11
  eOD-GT8)").

- source_assay:

  character. Assay that generated the row ("flow", "sequencing", or
  "flow and sequencing").

- endpoint:

  character. Full description of the measured endpoint (e.g. "Number of
  B cells", "Percent of IgG+ B cells that are GT8++").

- endpoint_value:

  numeric. Measured value of the endpoint (count or percent depending on
  endpoint_value_type).

- endpoint_value_type:

  character. Type of endpoint value ("count" or "percent").

- endpoint_value_imputed:

  numeric. Endpoint value after imputation of values below the limit of
  detection; equals endpoint_value when no imputation was needed.

- bcell_population:

  character. B-cell population label (e.g. "GT8++ IgG+ B cells",
  "epitope-specific (KO-GT8++) IgG+ B cells").

- percent_denominator:

  character. Parent population used as the denominator for percent
  endpoints (NA for count endpoints).

- igx_type:

  character. Immunoglobulin isotype classification (e.g. "IgG+"; NA when
  not applicable).

- antigen_specificity:

  character. Antigen-binding specificity classification (e.g. "GT8++";
  NA when not applicable).

- epitope_specificity:

  character. Epitope-level specificity based on KO probe binding (e.g.
  "KO-"; NA when not applicable).

- bnab_class:

  character. Broadly neutralizing antibody class assignment (e.g.
  "VRC01-class"; NA when not applicable).

- source_file:

  character. URL of the original source file on GitHub.

## Source

<https://github.com/SchiefLab/G001/raw/main/data/figures/flow_summary/flow_and_sequences.csv.gz>
