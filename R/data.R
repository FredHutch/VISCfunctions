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
#'   \item{antigen}{antigen}
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
#'   \item{isolate}{isolate}
#'   \item{titer_mod_50}{truncated response magnitude value, ID50 titer (min. 10)}
#'   \item{titer_mod_80}{truncated response magnitude value, ID80 titer (min. 10)}
#'   \item{response_50}{response call for ID50 titer (0/1)}
#'   \item{response_80}{response call for ID80 titer (0/1)}
#'   ...
#' }
"exampleData_NAb"
