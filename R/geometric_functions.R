#' Functions for Log-Scale Transformations - Geometric Mean, Geometric Median, Geometric Standard Deviation and Geometric Quantiles
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Compute the sample geometric mean, standard deviation, quantiles, and median. These functions are more suitable "averages" than the arithmetic averages for data that is on a log (geometric) scale. There are four functions:
#' * `geomean()` returns the geometric mean
#' * `geomedian()` returns the geometric median
#' * `geosd()` returns the geometric standard deviation
#' * `geoquantile()` returns the geometric quantiles
#' @aliases geomedian(), geoquantile(), geosd()
#' @param x Numeric vector. Must have length greater than 2, and contain only positive values.
#' @param na.rm Logical scalar indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric mean. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#' @param probs (geoquantile only) Numeric vector of probabilities between 0 and 1 for specifying which quantiles should be returned.
#' @param type (geoquantile only) Integer scalar between 1 and 9 selecting one of the nine quantile algorithms. Default is type 2, the post-2010 SAS default, which uses the inverse of the empirical distribution function averaging at discontinuities.
#' @param ... Additional arguments passed to \code{stats::quantile()} (geoquantile only).
#'
#' @return Returns a numeric scalar with sample geometric statistic for [geomean()], [geomedian()] and [geosd()]. Returns a numeric vector the length of 'probs' for [geoquantile()].
#'
#' @seealso \code{\link[base:mean]{mean}}, \code{\link[stats:median]{median}}, \code{\link[stats:quantile]{quantile}}, \code{\link[stats:sd]{sd}}, for the related arithmetic functions
#'
#' @details
#' Each function takes a vector of positive numbers, log-transforms them, computed the given statistic, and then transforms the result back to the original scale. \code{geoquantile()} requires both a vector of probabilities and the quantile method type. See \code{quantile()} for details on methods specified by type. \code{geomedian()} is a wrapper for \code{geoquantile(x, probs = 0.5, type = 2)}.
#'
#' @examples
#' # Linear and Exponential Data
#' x <- 1:20
#' y <- exp(x)
#'
#' # Arithmetic mean and geometric mean
#' geomean(x)
#' mean(x)
#' geomean(y)
#' mean(y)
#'
#' # Standard deviations
#' geosd(x)
#' stats::sd(x)
#' geosd(y)
#' stats::sd(y)
#'
#' # Quantiles
#' geomedian(x)
#' median(x)
#' geomedian(y)
#' median(y)
#' geoquantile(x)
#' fivenum(x)


#' @export
#' @rdname geomean
geomean <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) stop('"x" must be a numeric vector.')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')

  if (na.rm) { x <- x[!is.na(x)] }
  exp(mean(log(x), na.rm = na.rm))

}


#' @export
#' @rdname geomean
geosd <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) stop('"x" must be a numeric vector.')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')

  if (na.rm) { x <- x[!is.na(x)] }
  exp(stats::sd(log(x), na.rm = na.rm))

}


#' @export
#' @rdname geomean
geoquantile <- function(
    x,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    type = 2,
    na.rm = TRUE,
    ...
) {

  if (!is.numeric(x)) stop('"x" must be a numeric vector.')
  if (length(x) < 2) stop('"x" must have a length more than two.')
  if (!is.logical(na.rm)) stop('"na.rm" must be logical (i.e., TRUE or FALSE).')

  # quantile-specific input checks
  if (!is.numeric(probs)) stop('"probs" must be numeric.')
  if (any(probs < 0) || any(probs > 1)) stop('"probs" must be between 0 and 1.')
  if (!is.numeric(type) || length(type) != 1) stop('"type" must be a single numeral.')
  if (type < 1 || type > 9) stop('"type" must be a numeral between 1 and 9.')

  if (na.rm) { x <- x[!is.na(x)] }
  exp(stats::quantile(log(x), probs = probs, na.rm = na.rm, type = type, ...))

}


#' @export
#' @rdname geomean
geomedian <- function(x, na.rm = TRUE) {

  geoquantile(
    x = x,
    probs = 0.5,
    type = 2,
    na.rm = na.rm
  )

}
