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
#'
#' @references
#' @seealso
#' @examples
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
#' @return a numeric scalar - the sample geometric median.
#'
#' @details
#' @references
#' @seealso
#' @examples
#' @export
#'
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
#' @return a numeric scalar - the sample geometric standard deviation.
#'
#' @details
#' @examples
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

