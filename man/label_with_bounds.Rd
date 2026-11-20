\name{label_with_bounds}
\alias{label_with_bounds}
\title{Format axis labels with optional boundary signs}
\usage{
label_with_bounds(formatter = as.character, lower = FALSE, upper = TRUE)
}
\arguments{
\item{formatter}{A function taking numeric breaks and returning a character vector of the same length, such as \code{scales::label_percent()}.}
\item{lower, upper}{Single non-missing logical values indicating whether to prefix the smallest and largest finite breaks with inequality signs.}
}
\value{A function accepting numeric breaks and returning character labels.}
\description{Combine a character labelling function with optional lower and upper boundary signs.}
\details{Boundaries are identified from original numeric breaks before formatting. All tied extrema are labelled. Non-finite breaks and missing formatted labels receive no sign. Requesting both bounds with only one distinct finite break raises an error. Empty input returns an empty character vector without calling the formatter. This function changes labels only: set breaks explicitly to match intended limits, and configure scale limits and out-of-bounds handling separately. Expression-returning formatters are not supported.}
\examples{
label_with_bounds(lower = TRUE, upper = TRUE)(c(0, 0.5, 1))
label_with_bounds(function(x) paste0(100 * x, "%"),
                  lower = TRUE, upper = TRUE)(c(0, 0.5, 1))
}
