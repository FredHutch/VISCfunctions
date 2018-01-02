#' Combining Two Vectors, Removing Completely Missing Rows, with P Value Returns in Certain Cases
#'
#' @param x numeric vector (can include NA values)
#' @param y numeric vector (can include NA values)
#' @param y_type is y a binary/group variable or continuous variable. This will impact if an p value of NA or 1 is returned when y has only 1 distinct value.
#' @param verbose a logical variable indicating if warnings and messages should be displayed
#'
#' @return A data.frame with all completely NA rows removed or a NA or 1 pvalue if no complete cases or only one distinct value
#'
#' @examples
#' x <- c(1:10,NA,100)
#' y_bin <- c(rep(1:2,6))
#' y_cont <- c(1:10,NA,100)
#' .rm_na_and_check(x, y_bin)
#' .rm_na_and_check(x, y_cont, y_type = 'continuous')
#'

.rm_na_and_check = function(x, y, y_type = c('binary', 'continuous'), verbose = FALSE){
  y_type <- match.arg(y_type)
  .check_numeric_input(x)
  if (y_type == 'binary') .check_binary_input(y) else .check_numeric_input(y)

  if (length(x) != length(y)) stop('"x" and "y" must be the same length')

  # Removing cases where x and y are both NA
  data_here <- data.frame(x,y)[!(is.na(x) & is.na(y)),]

  #Various scenarios where will give message and return p value (NA or 1) instead of data.frame
  if (all(is.na(x) | is.na(y))) {
    if (verbose) message('There are no observations with non-missing values of both "x" and "y", so p=NA returned')
    return(NA)
  }

  if (length(unique(data_here$x[!is.na(data_here$y)])) == 1) {
    if (verbose) message('"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')
    return(1)
  }

  if (length(unique(data_here$y[!is.na(data_here$x)])) == 1) {
    #if binary/group variable NA should be returned, but if continuous then p=1 returned
    if (y_type == 'binary') {
      if (verbose) message('"y" only has 1 level when considering non-missing values of x, so p=NA returned')
      return(NA)
    } else {
      if (verbose) message('"y" only has 1 distinct value when considering non-missing values of x, so p=1 returned')
      return(1)
    }
  }

  data_here
}



