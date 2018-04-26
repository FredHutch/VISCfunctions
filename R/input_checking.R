#' Checking Numeric Vector Input
#'
#' @param x numeric vector (can include NA values).
#' @param lower_bound numeric value indicating the lowest allowed value.
#' @param upper_bound numeric value indicating the highest allowed value.
#' @param scalar logical value indicating if the checked value should only had an length of 1
#' @param whole_num logical value indicating if the checked value should be a whole number
#'
#' @examples
#' example_vals <- c(1:10,NA,100)
#' .check_numeric_input(example_vals)
#' .check_numeric_input(letters[1:10])
#'

.check_numeric_input = function(x, lower_bound = NULL, upper_bound = NULL, scalar = FALSE, whole_num = FALSE){
  param_name <- deparse(match.call()[[2]])
  if (length(dim(x)) > 1) stop('"', param_name, '" must be a vector (one-dimensional object)')
  if (length(x) == 0) stop('"', param_name, '" length must be > 0')
  if (scalar & length(x) != 1) stop('"', param_name, '" length must be 1 since expecting scalar')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop('"', param_name, '" must have at least one non-NA value')
  if (!is.numeric(x)) stop('"', param_name, '" must be a numeric vector')
  if (!is.null(lower_bound) & any(x < lower_bound)) stop('"', param_name, '" must be greater than or equal to ', lower_bound)
  if (!is.null(upper_bound) & any(x > upper_bound)) stop('"', param_name, '" must be less than or equal to ', upper_bound)
  if (whole_num & any(x %% 1 != 0)) stop('"', param_name, '" must be whole number(s)')
}


#' Checking Binary Vector Input
#'
#' @param x binary vector (can include NA values)
#' @param paired a logical indicating whether to add paired testing checking
#'
#' @examples
#' example_vals <- c(rep(0:1,10), NA)
#' .check_binary_input(example_vals)
#' .check_binary_input(letters[1:10])
#'

.check_binary_input = function(x, paired = FALSE){
  param_name <- deparse(match.call()[[2]])
  if (length(dim(x)) > 1) stop('"', param_name, '" must be a vector (one-dimensional object)')
  if (length(x) == 0) stop('"', param_name, '" length must be > 0')
  if (paired & any(is.na(x))) stop('When "paired" = TRUE "', param_name, '" cannot have missing values')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop('"', param_name, '" must have at least one non-NA value')
  if (length(unique(x)) > 2) stop('"', param_name, '" cannot have more than 2 distinct values')
  if (paired & length(unique(x)) == 1) stop('When "paired" = TRUE ', '"', param_name, '" must have exactly 2 distinct values')
  if (paired & sum(x == unique(x)[1]) != sum(x == unique(x)[2])) stop('When "paired" = TRUE "', param_name, '" must have the same number of samples for each level')
}

#' Checking Response Vector Input
#'
#' @param x response vector either 0/1 numeric or F/T logical values (can include NA values)
#'
#' @examples
#' example_vals <- c(rep(0:1,10), NA)
#' .check_response_input(example_vals)
#' .check_response_input(letters[1:10])
#'

.check_response_input = function(x){
  param_name <- deparse(match.call()[[2]])
  if (length(dim(x)) > 1) stop('"', param_name, '" must be a vector (one-dimensional object)')
  if (length(x) == 0) stop('"', param_name, '" length must be > 0')
  x <- x[!is.na(x)]
  if (length(x) == 0) stop('"', param_name, '" must have at least one non-NA value')
  if (!is.logical(x) & !all(x %in% c(NA,0,1))) stop('"', param_name, '" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
}





