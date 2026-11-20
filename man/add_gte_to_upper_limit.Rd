\name{add_gte_to_upper_limit}
\alias{add_gte_to_upper_limit}
\alias{add_lte_to_lower_limit}
\title{Add inequality signs to axis limit labels}
\usage{
add_gte_to_upper_limit(breaks)

add_lte_to_lower_limit(breaks)
}
\arguments{
\item{breaks}{A numeric vector of axis breaks.}
}
\value{A character vector with the same length and order as \code{breaks}. Missing and non-finite breaks are preserved without an inequality sign.}
\description{Label the smallest or largest finite break with an inequality sign. These functions can be supplied directly to the \code{labels} argument of a ggplot2 continuous scale.}
\details{These helpers change labels only, not data or scale limits. Set breaks explicitly so the extreme break equals the intended assay or plotting limit. Configure out-of-bounds handling separately, for example with \code{oob = scales::squish}. With a single finite break, that break is labelled. All occurrences of a tied extreme receive the sign.}
\examples{
add_gte_to_upper_limit(c(0, 10, 20, 30))
add_lte_to_lower_limit(c(100, 1000, 10000))
}
