#' Continuous Variable Compared to Binary Variable Test (VISC)
#'
#' Either Wilcox or T-Test Performed, for unpaired or paired data
#'
#' @param x numeric vector (can include NA values)
#' @param y vector with only 2 levels (can include NA values unless \code{paired = TRUE})
#' @param method what test to run (wilcox or t test)
#' @param paired a logical indicating whether you want a paired test.
#' @param verbose a logical variable indicating if warnings and messages should be displayed
#' @param ... parameters to pass to wilcox_test or t.test functions. For example the testing direction (\code{alternative}) in either call or the \code{var.equal} in the t.test function.
#' @return pvalue for comparing x at the different levels of y
#' @details
#'
#' Runs \code{wilcox_test()} in the coin package, with "exact" distribution and mid-ranks ties method.
#'
#' For one sided tests if y is a factor variable the level order is respected, otherwise the levels will set to alphabetical order (i.e. if \code{alternative = less} then testing a < b )
#'
#' If paired = TRUE assumes the first observations of the first group matches the first observation of the second group, and so on. Also if \code{paired = TRUE} then \code{y} must have the same number of samples for each level.
#'
#' @examples
#'
#' set.seed(5432322)
#' x <- c(rnorm(10,0,3), rnorm(10,3,3))
#' y <- c(rep('a', 10), rep('b', 10))
#' two_samp_cont_test(x = x, y = y, method = 'wilcox', paired = FALSE)
#' two_samp_cont_test(x = x, y = y, method = 'wilcox', paired = TRUE)
#' two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE)
#' two_samp_cont_test(x = x, y = y, method = 't', paired = TRUE)
#'
#' @export


two_samp_cont_test <- function(x, y, method = c('wilcox', 't'), paired = FALSE, verbose = FALSE, ...){
  # Input checking
  method <- match.arg(method)
  .check_numeric_input(x)
  .check_binary_input(y, paired = paired)
  y <- droplevels(factor(y))

  if (length(x) != length(y)) stop('"x" and "y" must be the same length')

  # Removing cases where x and y are both NA
  data_here <- data.frame(x,y)[!(is.na(x) & is.na(y)),]
  if (nrow(data_here) == 0 | all(is.na(x) | is.na(y))) {
    if (verbose) message('There are no observations with non-mising values of both "x" and "y", so p=NA returned')
    return(NA)
  }

  if (length(unique(data_here$x[!is.na(data_here$y)])) == 1) {
    if (verbose) message('"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')
    return(1)
  }

  if (length(unique(data_here$y[!is.na(data_here$x)])) == 1) {
    if (verbose) message('"y" only has 1 level when considering non-missing values of x, so p=NA returned')
    return(NA)
  }

  if (method == 'wilcox') {
    if (paired) {
      as.double(coin::pvalue(coin::wilcoxsign_test(data_here$x[data_here$y == levels(data_here$y)[1]] ~ data_here$x[data_here$y == levels(data_here$y)[2]], distribution = "exact", zero.method = "Pratt", ...)))
    } else {
      as.double(coin::pvalue(coin::wilcox_test(x ~ y, data = data_here, distribution = "exact", ties.method = "mid-ranks", ...)))
    }
  } else {
    #If both groups have only one distinct value t.test will throw error
    if (any(by(data_here$x[!is.na(data_here$y)], data_here$y[!is.na(data_here$y)], function(xx) {length(unique(xx[!is.na(xx)])) > 1}))) {
      as.double(t.test(data_here$x[data_here$y == levels(data_here$y)[1]], data_here$x[data_here$y == levels(data_here$y)[2]], data = data_here, paired = paired, ...)$p.value)
    } else {
      if (verbose) message('t.test can not run when both levels of "y" have only 1 unique "x" value, so p=NA returned')
      NA
    }
  }
}
