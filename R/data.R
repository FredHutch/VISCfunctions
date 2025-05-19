#' Example BAMA dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) BAMA data for
#' 7 antigens, 2 groups, and 3 visits. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 252 rows and 6 variables:
#' \describe{
#'   \item{pubID}{randomized subject ID}
#'   \item{group}{study group}
#'   \item{visitno}{visit number: baseline, 1, and 2}
#'   \item{antigen}{antigen tested}
#'   \item{magnitude}{magnitude of response, continuous, MFI*}
#'   \item{response}{response call for MFI* (0/1, NA at baseline)}
#'   ...
#' }
"exampleData_BAMA"

#' Example ICS dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) ICS data for
#' 3 cell populations and 2 antigen stimulations. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 306 rows and 15 variables:
#' \describe{
#'   \item{pubID}{randomized subject ID}
#'   \item{Group}{study group}
#'   \item{Visit}{visit number: baseline, 1, and 2}
#'   \item{Stim}{antigen stimulation}
#'   \item{Parent}{T cell subset}
#'   \item{Population}{cell population}
#'   \item{Count}{cell count}
#'   \item{ParentCount}{parent cell count}
#'   \item{CountBG}{background cell count}
#'   \item{ParentCountBG}{parent background cell count}
#'   \item{PercentCell}{count / parent count}
#'   \item{PercentCellNet}{response magnitude: percent cell - background percent cell}
#'   \item{response_prob}{MIMOSA response probability}
#'   \item{response_fdr_P}{FDR-adjusted p-value}
#'   \item{response}{response call for percent cell net (0/1)}
#'   ...
#' }
"exampleData_ICS"

#' Example NAb dataset
#'
#' A dataset containing response (0/1) and magnitude (continuous) NAb data for
#' 6 isolates, 4 groups, and 1 visit. Can be used to run VISCfunctions examples.
#'
#' @format A data frame with 210 rows and 9 variables:
#' \describe{
#'   \item{pubID}{randomized subject ID}
#'   \item{group}{study group}
#'   \item{visitno}{visit number}
#'   \item{celltype}{cell type}
#'   \item{isolate}{isolate tested}
#'   \item{titer_mod_50}{truncated response magnitude value, ID50 titer (min. 10)}
#'   \item{titer_mod_80}{truncated response magnitude value, ID80 titer (min. 10)}
#'   \item{response_50}{response call for ID50 titer (0/1)}
#'   \item{response_80}{response call for ID80 titer (0/1)}
#'   ...
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

#' @title Example B-cell flow-and-summary file derived from G001
#' @description Example B-cell flow-and-summary file derived from G001
#' @format A data frame with 3995 rows and 23 variables:
#' \describe{
#'   \item{\code{PubID}}{character publication ID}
#'   \item{\code{Group}}{character group}
#'   \item{\code{Treatment}}{character treatment}
#'   \item{\code{dose}}{character dose}
#'   \item{\code{dose_unit}}{character dose unit}
#'   \item{\code{visitno}}{character visit number}
#'   \item{\code{visit}}{character visit}
#'   \item{\code{visitunits}}{character visit units}
#'   \item{\code{Week}}{integer week}
#'   \item{\code{bcell_population}}{character bcell population}
#'   \item{\code{igx_type}}{character igx type}
#'   \item{\code{antigen_specificity}}{character antigen specificity}
#'   \item{\code{epitope_specificity}}{character epitope specificity}
#'   \item{\code{bnab_class}}{character bnab class}
#'   \item{\code{source_assay}}{character source assay}
#'   \item{\code{flag_bound}}{logical flag bound}
#'   \item{\code{probeset}}{logical probe set}
#'   \item{\code{sample_type}}{character sample type}
#'   \item{\code{source_file}}{character source file}
#'   \item{\code{name}}{character name}
#'   \item{\code{value}}{double value}
#'   \item{\code{value_type}}{character value type}
#'   \item{\code{percent_denominator}}{character percent denominator}
#'}
#' @details Example B-cell flow-and-summary file derived from G001
#' @source \url{https://github.com/SchiefLab/G001/raw/main/data/figures/flow_summary/flow_and_sequences.csv.gz}
"G001_fas"
