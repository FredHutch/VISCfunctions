% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{exampleData_ICS}
\alias{exampleData_ICS}
\title{Example ICS dataset}
\format{
A data frame with 306 rows and 15 variables:
\describe{
\item{pubID}{randomized subject ID}
\item{Group}{study group}
\item{Visit}{visit number: baseline, 1, and 2}
\item{Stim}{antigen stimulation}
\item{Parent}{T cell subset}
\item{Population}{cell population}
\item{Count}{cell count}
\item{ParentCount}{parent cell count}
\item{CountBG}{background cell count}
\item{ParentCountBG}{parent background cell count}
\item{PercentCell}{count / parent count}
\item{PercentCellNet}{response magnitude: percent cell - background percent cell}
\item{response_prob}{MIMOSA response probability}
\item{response_fdr_P}{FDR-adjusted p-value}
\item{response}{response call for percent cell net (0/1)}
...
}
}
\usage{
exampleData_ICS
}
\description{
A dataset containing response (0/1) and magnitude (continuous) ICS data for
3 cell populations and 2 antigen stimulations. Can be used to run VISCfunctions examples.
}
\keyword{datasets}
