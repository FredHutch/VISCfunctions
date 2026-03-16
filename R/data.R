#' Example BAMA dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) BAMA data for
#' 7 antigens, 2 groups, and 3 visits. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 252 rows and 6 variables:
#' \describe{
#'   \item{pubID}{character. Randomized (de-identified) subject ID.}
#'   \item{group}{integer. Study group (1 or 2).}
#'   \item{visitno}{numeric. Visit number (0 = baseline, 1, 2).}
#'   \item{antigen}{character. Name of the antigen tested.}
#'   \item{magnitude}{numeric. Response magnitude (MFI-blank, background-subtracted
#'     median fluorescence intensity).}
#'   \item{response}{integer. Response call based on MFI-blank (1 = responder,
#'     0 = non-responder, NA at baseline).}
#' }
"exampleData_BAMA"

#' Example ICS dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) ICS data for
#' 3 cell populations and 2 antigen stimulations. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 306 rows and 15 variables:
#' \describe{
#'   \item{pubID}{character. Randomized (de-identified) subject ID.}
#'   \item{Group}{character. Study group.}
#'   \item{Visit}{integer. Visit number (0 = baseline, 1, 2).}
#'   \item{Stim}{character. Antigen stimulation name (e.g. GAG, POL).}
#'   \item{Parent}{character. T cell subset used as the parent gate (e.g. CD4/TOTM).}
#'   \item{Population}{character. Cytokine-expressing cell population (e.g. IFNg, IFNg Or IL2).}
#'   \item{Count}{integer. Number of cytokine-positive cells in the stimulated sample.}
#'   \item{ParentCount}{integer. Total number of parent T cells in the stimulated sample.}
#'   \item{CountBG}{integer. Number of cytokine-positive cells in the background (unstimulated) sample.}
#'   \item{ParentCountBG}{integer. Total number of parent T cells in the background sample.}
#'   \item{PercentCell}{numeric. Percent cytokine-positive cells in the stimulated sample
#'     (Count / ParentCount).}
#'   \item{PercentCellNet}{numeric. Net (background-subtracted) percent cytokine-positive cells;
#'     primary response magnitude measure (PercentCell - background PercentCell).}
#'   \item{response_prob}{numeric. MIMOSA posterior probability of a positive response.}
#'   \item{response_fdr_P}{numeric. FDR-adjusted p-value from MIMOSA.}
#'   \item{response}{numeric. Response call based on PercentCellNet (1 = responder,
#'     0 = non-responder).}
#' }
"exampleData_ICS"

#' Example NAb dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) NAb data for
#' 6 isolates, 4 groups, and 1 visit. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 210 rows and 9 variables:
#' \describe{
#'   \item{pubID}{character. Randomized (de-identified) subject ID.}
#'   \item{group}{numeric. Study group (2, 3, 4, or 5).}
#'   \item{visitno}{integer. Visit number.}
#'   \item{celltype}{character. Cell line used in the neutralization assay (e.g. TZM-bl).}
#'   \item{isolate}{character. Name of the HIV isolate (pseudovirus) tested.}
#'   \item{titer_mod_50}{numeric. ID50 neutralization titer, truncated at a minimum of 10
#'     (below-detection values set to 10).}
#'   \item{titer_mod_80}{numeric. ID80 neutralization titer, truncated at a minimum of 10
#'     (below-detection values set to 10).}
#'   \item{response_50}{numeric. Response call based on ID50 titer (1 = responder,
#'     0 = non-responder).}
#'   \item{response_80}{numeric. Response call based on ID80 titer (1 = responder,
#'     0 = non-responder).}
#' }
"exampleData_NAb"

#' Example mAB dataset from DataSpace
#'
#' A subset dataset from the Farzan CAVD 812 Study, looking at eCD4 neutralization.
#'
#' @format A data frame with 480 rows and 4 variables:
#' \describe{
#'   \item{product}{Eight eCD4-Ig variants were tested}
#'   \item{virus}{60 HIV pseudoviruses from 12 different clades}
#'   \item{ic50}{ID50 titer}
#'   \item{ic80}{ID80 titer}
#' }
#' @source \url{https://dataspace.cavd.org}
"CAVD812_mAB"

#' Example B-cell flow and sequencing dataset from G001
#'
#' A subset of the B-cell flow cytometry and BCR sequencing summary file from
#' the G001 clinical trial (germline-targeting HIV vaccine). Contains PBMC
#' samples only.
#'
#' @format A data frame with 3995 rows and 22 variables:
#' \describe{
#'   \item{pubid}{character. Randomized (de-identified) participant ID.}
#'   \item{group}{integer. Study group (1 = low dose, 2 = high dose; NA for placebo).}
#'   \item{treatment}{character. Treatment description (e.g. "20 µg eOD-GT8 60mer + AS01B",
#'     "DPBS sucrose").}
#'   \item{dose}{numeric. Vaccine dose in micrograms (NA for placebo).}
#'   \item{dose_unit}{character. Unit for the dose field (µg); NA for placebo.}
#'   \item{visitno}{character. Visit identifier as a zero-padded string (e.g. "02", "06").}
#'   \item{visit}{integer. Visit time point in weeks relative to first vaccination
#'     (negative = pre-vaccination).}
#'   \item{visit_units}{character. Units for the visit column (always "weeks").}
#'   \item{sample_type}{character. Biological sample type (always "PBMC").}
#'   \item{probeset}{character. Label for the probe/stain panel used (e.g.
#'     "G001 PBMC (KO11 eOD-GT8)").}
#'   \item{source_assay}{character. Assay that generated the row
#'     ("flow", "sequencing", or "flow and sequencing").}
#'   \item{endpoint}{character. Full description of the measured endpoint (e.g.
#'     "Number of B cells", "Percent of IgG+ B cells that are GT8++").}
#'   \item{endpoint_value}{numeric. Measured value of the endpoint (count or percent
#'     depending on endpoint_value_type).}
#'   \item{endpoint_value_type}{character. Type of endpoint value ("count" or "percent").}
#'   \item{endpoint_value_imputed}{numeric. Endpoint value after imputation of values
#'     below the limit of detection; equals endpoint_value when no imputation was needed.}
#'   \item{bcell_population}{character. B-cell population label (e.g. "GT8++ IgG+ B cells",
#'     "epitope-specific (KO-GT8++) IgG+ B cells").}
#'   \item{percent_denominator}{character. Parent population used as the denominator for
#'     percent endpoints (NA for count endpoints).}
#'   \item{igx_type}{character. Immunoglobulin isotype classification (e.g. "IgG+"; NA
#'     when not applicable).}
#'   \item{antigen_specificity}{character. Antigen-binding specificity classification
#'     (e.g. "GT8++"; NA when not applicable).}
#'   \item{epitope_specificity}{character. Epitope-level specificity based on KO probe
#'     binding (e.g. "KO-"; NA when not applicable).}
#'   \item{bnab_class}{character. Broadly neutralizing antibody class assignment
#'     (e.g. "VRC01-class"; NA when not applicable).}
#'   \item{source_file}{character. URL of the original source file on GitHub.}
#' }
#' @source \url{https://github.com/SchiefLab/G001/raw/main/data/figures/flow_summary/flow_and_sequences.csv.gz}
"G001_Bcell_flow_seq_PBMC"
