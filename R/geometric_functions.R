#' Functions for Log-Scale Transformations - Geometric Mean, Geometric Median, Geometric Standard Deviation and Geometric Quantiles
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes the sample geometric mean, standard deviation quantiles and median. These functions are more suitable "averages" than the arithmetic averages for data that is on a log (geometric) scale. There are four functions:
#' * `geomean()` returns the geometric mean
#' * `geomedian()` returns the geometric median
#' * `geosd()` returns the geometric standard deviation
#' * `geoquantile()` return the geometric quantiles
#' @aliases geomedian(), geoquantile(), geosd()
#' @param x Numeric vector. Vector must be longer than two elements.
#' @param  na.rm Logical scalar indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric mean. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#' @param threshold Positive scalar indicating a lower bound for 'x'. If 'threshold = NULL', no threshold will be set and the data will remain unaltered. If the threshold is set to any positive number (the default is 1) all values below the threshold will be converted to the value in 'threshold'. If 'threshold = NULL' and 'x' contains zero or negative values, then an error is returned.
#' @param probs (geoquantile only) Numeric vector of probabilities between 0 and 1 for specifying which quantiles should be returned.
#' @param type (geoquantile only) Integer scalar between 1 and 9 selecting one of the nine quantile algorithms. Default is type 2, the post-2010 SAS default, which uses the inverse of the empirical distribution function averaging at discontinuities.
#' @param verbose Logical scalar indicating if a warning should appear if there are values in 'x' that are less than 1 and the threshold is set to NULL or less than 1.
#' @param ... Additional arguments passed to functions
#'
#' @return Returns a numeric scalar with sample geometric statistic for [geomean()], [geomedian()] and [geosd()]. Returns a numeric vector the length of 'probs' for [geoquantile()]
#'
#' @seealso \code{\link[base:mean]{mean}}, \code{\link[stats:median]{median}}, \code{\link[stats:quantile]{quantile}}, \code{\link[stats:sd]{sd}}, for the related arithmetic functions
#'
#' @details
#' Each function takes a vector of non-negative numbers, log-transforms the numbers, finds the statistic for the numbers and then transforms the result back to the normal scale. Zero and negative numbers must be changed to positive numbers. This can be handled using the 'threshold'. The default for threshold is 1, which when log-transformed becomes zero. \code{geoquantile()} requires both a vector of probabilities and the quantile method type. See \code{quantile()} for details on methods specified by type. \code{geomedian()} is a wrapper for \code{geoquantile(x, probs = 0.5, type = 2)}.
#' @examples
#' # Linear and Exponential Data
#' x <- 1:20
#' y <- exp(x)
#'
#' #Arithemetic mean and geometric mean
#' geomean(x)
#' mean(x)
#'
#' geosd(x)
#' stats::sd(x)
#'
#' geomedian(x)
#' median(x)
#'
#' geoquantile(x)
#' fivenum(x)
#'
#' geomean(y)
#' mean(y)
#'
#' # Data with zero and negative values
#' x[c(2, 10, 15)] <- NA
#' x[c(4, 12, 17)] <- -1*x[c(4, 12, 17)]
#' x[7] <- 0
#' x[1] <- 0.2
#'
#' #The default is for 'NA' to be removed and a threshold is set at 1 to
#' #  transform any values below 1 to 1.
#' geomean(x, na.rm = TRUE, threshold = 1)
#'
#' #The threshold can be removed by setting it to 'NULL'. This will generate an
#' #  error if there are any zero or negative values.
#' \dontrun{
#' geomedian(x, na.rm = FALSE)
#' geomean(x, threshold = NULL)
#' geosd(x, threshold = 0.1, verbose = TRUE)
#' }
#'
#' geoquantile(x <- rnorm(1001)) # Extremes & Quartiles by default
#' geoquantile(x,  probs = c(0, 0.01, 0.1, 1))
#' geoquantile(x, type = 9)
#'
#' @export geomean
#' @describeIn Geometric Mean for Log-transformed Data
geomean <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE)
  {# Input Checking
  # must be a numeric vector, not a factor
  if (!is.numeric(x)) stop ('"x" must be a numeric vector.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  #threshold options
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }else{
    if (any(x <= 0)) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }
if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceding zeros. Thresholds set to one will become zero upon log transformation.")}

  # Conversions
  # set the values less than the threshold to the threshold
  if (!is.null(threshold)) { x[x <= threshold] <- threshold}
  # decrease the length of 'x' if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {x <- x[!is.na(x)]}
  # Function
  #
  exp(mean(log(x), na.rm = na.rm))
}
#' @export geoquantile
#' @describeIn Geometric Quantiles for Log-transformed Data
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
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(probs) < 1) stop('"probs" must have a length of at least one.')
  if (any(probs < 0) | any(probs > 1)) stop('"probs" must have a must be between 0 and 1.')
  if (type < 1 | type > 9) stop('"type" must be a numeral between 1 and 9.')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }else{
    if (any(x <= 0)) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }
  if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceding zeros. Thresholds set to one will become zero upon log transformation.")}
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
  exp(stats::quantile(log(x), probs = probs, na.rm = na.rm, type = type))
}
#' @export geomedian
#' @describeIn Geometric Median for Log-transformed Data
geomedian <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE
){geoquantile(x = x,
               na.rm = na.rm,
               threshold = threshold,
               verbose = verbose,
               type = 2,
               probs = 0.5)
}

#' @export geosd
#' @describeIn Geometric Standard Deviation for Log-transformed Data
geosd <- function(
  x,
  na.rm = TRUE,
  threshold = 1L,
  verbose = FALSE
){# Input Checking
  requireNamespace("stats")
  # must be a numeric vector, not a factor
  if (!is.numeric(x)) stop ('"x" must be a numeric vector.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')
  if (!(is.numeric(threshold)|is.null(threshold))) stop('"threshold" must be numeric or null')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.null(threshold)){
    if (length(threshold) != 1) stop('"threshold" must have a length of one.')
    if (threshold > max(x, na.rm = TRUE))  stop('"threshold" must be less than at least one value of "x"')
    if (any(x <= 0) & threshold <= 0) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }else{
    if (any(x <= 0)) stop('"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  }
  if (verbose == TRUE & any(x < 1) & (is.null(threshold)||threshold < 1)){
    # Explain thresholds less than one
    warning("Any non-null thresholds with values less than one will generate negative numbers when log-transformed. These negative numbers can change the summary statistics in unexpected ways, especially if the other data values are much larger than one or the threshold contains a lot of decimal places with preceding zeros. Thresholds set to one will become zero upon log transformation.")}
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
  exp(stats::sd(log(x), na.rm = na.rm))
}
