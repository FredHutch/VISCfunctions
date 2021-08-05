#' Functions for Log-Scale Transformations - Geometric Mean, Median, Standard Deviation and Quantiles

#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Computes the sample geometric mean, standard deviation quantiles and median. These functions are more suitable "averages" than the arithmetic averages for data that is on a log (geometric) scale. There are four functions:
#' * `geomean()` returns the geometric mean
#' * `geomedian()` returns the geometric median
#' * `geosd()` returns the geometric standard deviation
#' * `geoquantile()` return the geometric quantiles
#'
#' @inheritParams
#' @param x Numeric vector of non-negative numbers on the the normal scale (i.e. not log-transformed). Vector must be longer than two elements.
#' @param  na.rm Logical scalar indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric mean. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#' @param threshold Logical scalar indicating a lower bound for 'x'. If 'threshold = NULL', no threshold will be set and the data will remain unaltered. If the threshold is set to any positive number (the default is 1) all values below the threshold will be converted to the value in 'threshold'. If 'threshold = NULL' and 'x' contains zero or negative values, then a missing value ('NA') is returned.
#' @param vebose Detailed description of the perils of setting a threshold less than one that appears if 'verbose = TRUE' and the 'threshold <1' and there are values less than 1 in 'x'.
#' @param probs (geoquantile only) Numeric vector of probabilities between 0 and 1 for specifying which quantiles should be returned.
#' @param type (geoquantile only) Integer scalar between 1 and 9 selecting one of the nine quantile algorithms. Default is type 2, the post-2010 SAS default, which uses the inverse of the empirical distribution function averaging at discontinuities.
#' @param ... Additional arguments passed to functions
#'
#' @return Numeric scalar with sample geometric statistic for [geomean()], [geomedian()] and [geosd()]. Numeric vector the length of 'probs' for [geoquantiles()]
#'
#' @seealso [mean()], [median()], [sd()], [fivenum()], [quantile()] for the related arithmetic functions
#'
#' @details
#' Each function takes a vector of non-negative numbers, log-transforms the numbers, finds the statistic for the numbers and then transforms the result back to the normal scale.
#' @examples
#' # Linear and Exponential Data
#' x <- seq(1:20)
#' y <- exp(x)
#'
#' Arithemetic mean and geometric mean give similar results for linear scale data
#' geomean(x)
#' mean(x)
#'
#' # Arithmetic mean and geometric mean give very different results for log-scale data
#' geomean(y)
#' mean(y)
#'
#' # Data with zero and negative values
#' x[c(2, 10, 15)] <- NA
#' x[c(4, 12, 17)] <- -1*x[c(4, 12, 17)]
#' x[7] <- 0
#' will produce an 'NA' result due to NA values in 'x'
#' geomean(x, na.rm = FALSE)
#' will produce an error due to zero and negative values in 'x'
#' geomean(x, threshold = NULL)
#' Setting the threshold at 1 works out to zero on the log scale, but there may be different assay thresholds.
#' geomean(x, threshold = 0.1, verbose = TRUE)
#'
#'
#' @export


geomean <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE
){# Input Checking
  # must be a numeric vector, not a factor
  if (!is.numeric(x)) stop ('"x" must be a numeric vector.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  #threshold options
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero.')
  }
if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceeding zeros. Thresholds set to one will become zero upon log transformation.")}

  # Conversions
  # set the values less than the threshold to the threshold
  if (!is.null(threshold)) { x[x <= threshold] <- threshold}
  # decrease the length of 'x' if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {x <- x[!is.na(x)]}
  # Function
  #
  exp(mean(log(x), na.rm = na.rm))
}


geoquantile <- function(
  x,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  type = 2,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE,
  ...
){# Input Checking
  #
  # if the length of the vector is less than two, cannot compute mean
  # must be a numeric vector
  if (!is.numeric(x)) stop ('"x" must be a numeric vector.')
  if (!is.numeric(probs)) stop ('"probs" must be numeric.')
  if (is.logical(probs)) stop ('"probs" must be numeric.')
  if (!is.numeric(type)) stop ('"type" must be numeric.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (length(probs) < 1) stop('"probs" must have a length of at least one.')
  if (any(probs < 0) | any(probs > 1)) stop('"probs" must have a must be between 0 and 1.')
  if (type < 1 | type > 9) stop('"type" must be a numeral between 1 and 9.')
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero.')
  }
if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceeding zeros. Thresholds set to one will become zero upon log transformation.")}
  # Conversions
  # set the values less than the threshold to the threshold
  if (!is.null(threshold)) {
    x[x <= threshold] <- threshold
  }
  # decrease the length of 'x' if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(quantile(log(x), probs = probs, na.rm = na.rm, type = type))
}

geomedian <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE
){geoquantile(x = x,
               na.rm = na.rm,
               treshold = threshold,
               verbose = verbose,
               type = 2,
               probs = 0.5)
}

geosd <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE
){# Input Checking
  # must be a numeric vector, not a factor
  if (!is.numeric(x)) stop ('"x" must be a numeric vector.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero.')
  }
  if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceeding zeros. Thresholds set to one will become zero upon log transformation.")}
  # if there are any zero or negative numbers and threshold is null throw an error

  # Conversions
  # set the values less than the threshold to the threshold
  if (!is.null(threshold)) {
    x[x <= threshold] <- threshold
  }
  # decrease the length of 'x' if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(sd(log(x), na.rm = na.rm))
}
