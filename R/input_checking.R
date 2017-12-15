#' Checking Numeric Input
#'
#' @param x numeric vector (can include NA values)
#' @param lower_bound numeric value indicating the lowest allowed value
#' @param upper_bound numeric value indicating the highest allowed value
#'
#' @examples
#' example_vals <- c(1:10,NA,100)
#' .check_numeric_input(example_vals)
#' .check_numeric_input(letters[1:10])
#'

.check_numeric_input = function(x, lower_bound = NULL, upper_bound = NULL){
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' length must be > 0')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' must have non "NA" values')
  if (!is.numeric(x)) stop(deparse(match.call()[[2]]), ' must be a numeric vector')
  if (!is.null(lower_bound) & any(x < lower_bound)) stop(deparse(match.call()[[2]]), ' must be greater than ', lower_bound)
  if (!is.null(upper_bound) & any(x > upper_bound)) stop(deparse(match.call()[[2]]), ' must be less than ', upper_bound)
}


#' Checking Binary Input
#'
#' @param x binary vector (can include NA values)
#'
#' @examples
#' example_vals <- c(rep(0:1,10), NA)
#' .check_binary_input(example_vals)
#' .check_binary_input(letters[1:10])
#'

.check_binary_input = function(x){
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' length must be > 0')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' must have non "NA" values')
  if (length(unique(x)) > 2) stop(deparse(match.call()[[2]]), ' can not have more than 2 distinct values')
}

#' Checking Response Input
#'
#' @param x response vector either 0/1 numeric or F/T logical values (can include NA values)
#'
#' @examples
#' example_vals <- c(rep(0:1,10), NA)
#' .check_response_input(example_vals)
#' .check_response_input(letters[1:10])
#'

.check_response_input = function(x){
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' length must be > 0')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop(deparse(match.call()[[2]]), ' must have non "NA" values')
  if (!is.logical(x) & !all(x %in% c(NA,0,1))) stop(deparse(match.call()[[2]]), ' must contain only 0/1 or T/F values')
}





