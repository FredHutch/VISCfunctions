% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_comparisons.R
\name{cor_test_pairs}
\alias{cor_test_pairs}
\title{Correlation Testing for Multiple Endpoints/Terms}
\usage{
cor_test_pairs(
  x,
  pair,
  id,
  method = c("spearman", "pearson", "kendall"),
  n_distinct_value = 3,
  digits = 3,
  trailing_zeros = TRUE,
  exact = TRUE,
  seed = 68954857,
  nresample = 10000,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{numeric vector (can include NA values)}

\item{pair}{categorical vector which contains the levels to compare}

\item{id}{vector which contains the id information}

\item{method}{character string indicating which correlation coefficient
is to be used for the test ("pearson" (default), "kendall", or "spearman").}

\item{n_distinct_value}{number of distinct values in \code{x} each \code{pair} must
contain to be compared. The value must be >1, with a default of 3.}

\item{digits}{numeric value between 0 and 14 indicating the number of digits
to round the correlation estimate. The default is set to 3.}

\item{trailing_zeros}{logical indicating if trailing zeros should be included
in the descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to
\code{TRUE}, output is a character vector.}

\item{exact}{logical value indicating whether the "exact" method should be
used. Ignored if \code{method = "pearson"} or if \code{method = "spearman"} and there
are ties in \code{x} for either \code{pair}.}

\item{seed}{numeric value used to set the seed. Only used if
\code{method = "spearman"} and there are ties in \code{x} for either \code{pair}.}

\item{nresample}{positive integer indicating the number of Monte Carlo
replicates to used for the computation of the approximative reference
distribution. Defaults is set to 10,000. Only used when
\code{method = "spearman"} and there are ties in \code{x} for either \code{pair}.}

\item{verbose}{logical variable indicating whether warnings and messages
should be displayed.}

\item{...}{parameters passed to \code{stats::cor.test} or \code{coin:spearman_test}}
}
\value{
Returns a data frame of all possible pairwise correlations
with pair sizes greater than or equal to the minimum number of values
in pair, as set by \code{n_distinct_value}:
\itemize{
\item \code{Correlation} - Comparisons made
\item \code{NPairs} - number of non-missing pairs considered
\item \code{Ties} - are ties present in either variable
\item \code{CorrEst} - correlation estimates
\item \code{CorrTest} - correlation test p value
}
}
\description{
Takes a continuous variable and a categorical variable, and calculates the
Spearman, Pearson, or Kendall correlation estimate and p-value
between the categorical variable levels.
}
\details{
The p value is calculated using the \link{cor_test} function (see documentation
for method details)

If a pair has less than \code{n_distinct_value} non-missing values that pair
will be excluded from the comparisons. If a specific comparison has less than
\code{n_distinct_value} non-missing values to comparison the output will return an
estimate and the p-value set to NA.
}
\examples{

data_in <- data.frame(
  id = 1:10,
  x = c(-2, -1, 0, 1, 2,-2, -1, 0, 1, 2),
  y = c(4, 1, NA, 1, 4,-2, -1, 0, 1, 2),
  z = c(1, 2, 3, 4, NA,-2, -1, 0, 1, 2),
  v = c(rep(1,10)),
  aa = c(1:5,NA,NA,NA,NA,NA),
  bb = c(NA,NA,NA,NA,NA,1:5)
)
data_in_long <- tidyr::pivot_longer(data_in, -id)
cor_test_pairs(x = data_in_long$value,
                  pair = data_in_long$name,
                  id = data_in_long$id,
                  method = 'spearman')


# Examples with Real World Data
library(dplyr)

# BAMA Assay Data Example
data(exampleData_BAMA)

## Antigen Correlation
exampleData_BAMA \%>\%
filter(visitno != 0) \%>\%
group_by(group, visitno) \%>\%
 summarize(
   cor_test_pairs(x = magnitude, pair = antigen, id = pubID,
   method = 'spearman', n_distinct_value = 3, digits = 1, verbose = TRUE),
   .groups = 'drop'
          )

}
