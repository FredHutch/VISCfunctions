#' Functions for Log-Scale Transformations - Geometric Mean
#'
#' @description Computes the sample geometric mean. This is a more suitable "average" than the arithmetic median for data that is on a log (geometric) scale. Takes a vector of non-negative numbers, log-transforms the numbers, finds the mean of the numbers and then transforms the result back to the normal scale. This is equivalent to the nth root of the product of n values.
#'
#' @param x Numeric vector of non-negative numbers on the the normal scale (i.e. not log-transformed). Vector must be longer than two elements.
#'
#' @param  na.rm Logical scalar indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric mean. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#'
#' @param use_threshold Logical scalar indicating whether to convert negative values to the 'negative threshold'. If 'use_threshold = TRUE' (the default) negative values will be converted to the value in 'negative_threshold' (the default is 1). If 'use_threshold = FALSE' and 'x' contains zero or negative values, then a missing value ('NA') is returned.
#'
#' @param negative_threshold Numeric scalar indicating the value to use if there are zero or negative values in the data. The default value is 1.
#'
#' @return a numeric scalar - the sample geometric mean.
#'
#' @details
#' @examples
#' # Geometric Mean and Arithmetic Mean
#' x <- seq(1:20)
#' y <- exp(x)
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
#' geomean(x, use_threshold = FALSE)
#' Setting the threshold at 1 works out to zero on the log scale, but there may be different assay thresholds.
#' geomean(x, use_threshold = TRUE, negative_threshold = 1)
#'
#'
#' @export


geomean <- function(
  x,
  na.rm = TRUE,
  use_threshold = TRUE,
  negative_threshold = 1
){
  # Input Checking
  #
  # if the length of the vector is less than two, cannot compute mean
  if (length(x) < 2) stop('"x" must have a length more than two.')
  # must be a numeric vector
  if (!is.vector(x, mode = "numeric") || is.factor(x)) stop ('"x" must be a numeric vector.')
  if(!is.logical(use_threshold)) stop('"use_threshold" must be logical (i.e.
                                      TRUE or FALSE)')
  if(!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE)')
  if (!is.numeric(negative_threshold)) stop('"negative_threshold" must be numeric')
  # if there are any zero or negative numbers and use_threshold is false throw an error
  if(any(x <= 0 & use_threshold == FALSE)) stop('There are zeros or negative
                                                values in "x". Set "use_threshold"
                                                to TRUE and set a threshold using
                                                "negative_threshold" or remove
                                                zeros and negatives from "x"
                                                before calculating.')
  # Conversions
  #
  if (use_threshold) {
     x[x <= 0] <- negative_threshold
  }
  # decrease the length of 'x' by one if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(sum(log(x), na.rm=na.rm) / length(x))
}


#' Functions for Log-Scale Transformations - Geometric Median
#'
#' @description Computes the sample geometric median for a vector of non-monotonic and univariate values. This is a more suitable "average" than the arithmetic median for data that is on a log (geometric) scale. Takes a vector of non-negative numbers, log-transforms the numbers, finds the median of the numbers and then transforms the result back to the normal scale.
#'
#' @param x numeric vector of non-negative numbers on the the normal scale (i.e. not log-transformed).
#'
#' @param  na.rm a logical scale indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric mean. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#'
#'  @param use_threshold Logical scalar indicating whether to convert negative values to the 'negative threshold'. If 'use_threshold = TRUE' (the default) negative values will be converted to the value in 'negative_threshold' (the default is 1). If 'use_threshold = FALSE' and 'x' contains zero or negative values, then a missing value ('NA') is returned.
#'
#' @param negative_threshold Numeric scalar indicating the value to use if there are zero or negative values in the data. The default value is 1.
#'
#' @return a numeric scalar - the sample geometric median.
#'
#' @details
#' @examples
#' # Geometric Median and Arithmetic Median
#' x <- seq(1:20)
#' y <- exp(x)
#' Arithemetic median and geometric median give similar results for linear scale data
#' geomedian(x)
#' median(x)
#'
#' # Arithmetic median and geometric median give very different results for log-scale data
#' geomedian(y)
#' median(y)
#'
#' # Data with zero and negative values
#' x[c(2, 10, 15)] <- NA
#' x[c(4, 12, 17)] <- -1*x[c(4, 12, 17)]
#' x[7] <- 0
#' will produce an 'NA' result due to NA values in 'x'
#' geomedian(x, na.rm = FALSE)
#' will produce an error due to zero and negative values in 'x'
#' geomedian(x, use_threshold = FALSE)
#' Setting the threshold at 1 works out to zero on the log scale, but there may be different assay thresholds.
#' geomedian(x, use_threshold = TRUE, negative_threshold = 1)
#' @export


geomedian <- function(
  x,
  na.rm = TRUE,
  use_threshold = TRUE,
  negative_threshold = 1
){
  # Input Checking
  #
  # if the length of the vector is less than two, cannot compute mean
  if (length(x) < 2) stop('"x" must have a length more than two.')
  # must be a numeric vector
  if (!is.numeric(x)) stop ('"x" must be numeric.')
  if(!is.logical(use_threshold)) stop('"use_threshold" must be logical (i.e.
                                      TRUE or FALSE)')
  if(!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE)')
  if (!is.numeric(negative_threshold)) stop('"negative_threshold" must be numeric')
  # if there are any zero or negative numbers and use_threshold is false throw an error
  if(any(x <= 0 & use_threshold == FALSE)) stop('There are zeros or negative
                                                values in "x". Set "use_threshold"
                                                to TRUE and set a threshold using
                                                "negative_threshold" or remove
                                                zeros and negatives from "x"
                                                before calculating.')
  # Conversions
  #
  if (use_threshold) {
    x[x <= 0] <- negative_threshold
  }
  # decrease the length of 'x' by one if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(median(log(x), na.rm=na.rm))

}




#' Functions for Log-Scale Transformations - Geometric Standard Deviation
#'
#' @description Computes the sample geometric standard deviation. Takes a vector of non-negative numbers, log-transforms the numbers, finds the standard deviation of the numbers and then transforms the result back to the normal scale.
#'
#' @param x numeric vector of non-negative numbers on the the normal scale (i.e. not log-transformed).
#'
#' @param  na.rm a logical scale indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric standard deviation. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#'
#'  @param use_threshold Logical scalar indicating whether to convert negative values to the 'negative threshold'. If 'use_threshold = TRUE' (the default) negative values will be converted to the value in 'negative_threshold' (the default is 1). If 'use_threshold = FALSE' and 'x' contains zero or negative values, then a missing value ('NA') is returned.
#'
#' @param negative_threshold Numeric scalar indicating the value to use if there are zero or negative values in the data. The default value is 1.
#'
#' @return a numeric scalar - the sample geometric standard deviation.
#'
#' @details
#' @examples
#' # Geometric Standard Deviation and Arithmetic Standard Deviation
#' x <- seq(1:20)
#' y <- exp(x)
#' Arithemetic standard deviation and geometric standard deviation give similar results for linear scale data
#' geosd(x)
#' sd(x)
#'
#' # Arithmetic standard deviation and geometric standard deviation give very different results for log-scale data
#' geosd(y)
#' sd(y)
#'
#' # Data with zero and negative values
#' x[c(2, 10, 15)] <- NA
#' x[c(4, 12, 17)] <- -1*x[c(4, 12, 17)]
#' x[7] <- 0
#' will produce an 'NA' result due to NA values in 'x'
#' geosd(x, na.rm = FALSE)
#' will produce an error due to zero and negative values in 'x'
#' geosd(x, use_threshold = FALSE)
#' Setting the threshold at 1 works out to zero on the log scale, but there may be different assay thresholds.
#' geosd(x, use_threshold = TRUE, negative_threshold = 1)
#' @export


geosd <- function(
  x,
  na.rm = TRUE,
  use_threshold = TRUE,
  negative_threshold = 1
){
  # Input Checking
  #
  # if the length of the vector is less than two, cannot compute mean
  if (length(x) < 2) stop('"x" must have a length more than two.')
  # must be a numeric vector
  if (!is.numeric(x)) stop ('"x" must be numeric.')
  if(!is.logical(use_threshold)) stop('"use_threshold" must be logical (i.e.
                                      TRUE or FALSE)')
  if(!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE)')
  if (!is.numeric(negative_threshold)) stop('"negative_threshold" must be numeric')
  # if there are any zero or negative numbers and use_threshold is false throw an error
  if(any(x <= 0 & use_threshold == FALSE)) stop('There are zeros or negative
                                                values in "x". Set "use_threshold"
                                                to TRUE and set a threshold using
                                                "negative_threshold" or remove
                                                zeros and negatives from "x"
                                                before calculating.')
  # Conversions
  #
  if (use_threshold) {
    x[x <= 0] <- negative_threshold
  }
  # decrease the length of 'x' by one if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(sd(log(x), na.rm=na.rm))
}


#' Functions for Log-Scale Transformations - Geometric Quantiles
#'
#' @description Computes the sample geometric quantiles. Takes a vector of non-negative numbers, log-transforms the numbers, finds the quantile of the numbers and then transforms the result back to the normal scale.
#'
#' @param x Numeric vector of non-negative numbers on the the normal scale (i.e. not log-transformed).
#'
#' @param probs Numeric vector of probabilities between 0 and 1 for specifying which quantiles should be returned.
#'
#' @param type Integer scalar between 1 and 9 selecting one of the nine quantile algorithms. Default is type 2, the post-2010 SAS default, which uses the inverse of the empirical distribution function averaging at discontinuities.
#'
#'  @param  na.rm Logical scale indicating whether to remove missing values from 'x'. If 'na.rm = TRUE' (the default) missing values are removed from 'x' prior to computing the geometric quantile. If 'na.rm = FALSE' and 'x' contains missing values, then a missing value ('NA') is returned.
#'
#'  @param use_threshold Logical scalar indicating whether to convert negative values to the 'negative threshold'. If 'use_threshold = TRUE' (the default) negative values will be converted to the value in 'negative_threshold' (the default is 1). If 'use_threshold = FALSE' and 'x' contains zero or negative values, then a missing value ('NA') is returned.
#'
#' @param negative_threshold Numeric scalar indicating the value to use if there are zero or negative values in the data. The default value is 1.
#'
#' @return a numeric scalar - the sample geometric quantile.
#'
#' @details
#' See quantiles for other types.
#' @examples
#' # Geometric Quantile and Arithmetic Quantile
#'
#' x <- rnorm(1001)
#' y <- rlnorm(1001) #log-normal distribution
#' geoquantile(x) # Extremes & Quartiles by default
#' geoquantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)
#' geoquantile(y)
#'
#'
#' # Data with zero and negative values
#' x[c(2, 10, 15)] <- NA
#' x[c(4, 12, 17)] <- -1*x[c(4, 12, 17)]
#' x[7] <- 0
#' will produce an 'NA' result due to NA values in 'x'
#' geoquantile(x, na.rm = FALSE)
#' will produce an error due to zero and negative values in 'x'
#' geoquantile(x, use_threshold = FALSE)
#' Setting the threshold at 1 works out to zero on the log scale, but there may be different assay thresholds.
#' geoquantile(x, use_threshold = TRUE, negative_threshold = 1)
#' @export


geoquantile <- function(
  x,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  type = 2,
  na.rm = TRUE,
  use_threshold = TRUE,
  negative_threshold = 1,
  ...
){
  # Input Checking
  #
  # if the length of the vector is less than two, cannot compute mean
  if (length(x) < 2) stop('"x" must have a length more than two.')
  # must be a numeric vector
  if (!is.numeric(x)) stop ('"x" must be numeric.')
  if(!is.logical(use_threshold)) stop('"use_threshold" must be logical (i.e.
                                      TRUE or FALSE)')
  if(!is.logical(na.rm)) stop('"na.rm" must be logical (i.e. TRUE or FALSE)')
  if (!is.numeric(negative_threshold)) stop('"negative_threshold" must be numeric')
  # if there are any zero or negative numbers and use_threshold is false throw an error
  if(any(x <= 0 & use_threshold == FALSE)) stop('There are zeros or negative
                                                values in "x". Set "use_threshold"
                                                to TRUE and set a threshold using
                                                "negative_threshold" or remove
                                                zeros and negatives from "x"
                                                before calculating.')
  # Conversions
  #
  if (use_threshold) {
    x[x <= 0] <- negative_threshold
  }
  # decrease the length of 'x' by one if there are 'NA' values in 'x' and 'na.rm' is FALSE
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # Function
  #
  exp(quantile(log(x), probs = probs, na.rm=na.rm, type = type))
}

