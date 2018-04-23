#' Rounding Using Round Away From 0 Method
#'
#' round_away_0 takes a numeric vector, rounds them to a specified digit amount using the round away from 0 method for ties (i.e. 1.5). This is the SAS method for rounding.
#'
#' @param x numeric vector (can include NA values).
#' @param rounding_digits positive integer of length 1 between 1 and 14, giving the amount of digits to round to.
#' @param verbose a logical variable indicating if warnings and messages should be displayed.
#' @return numeric vector of rounded values.
#' @details
#'
#' \code{round_away_0} is not designed for use at precision levels <= 1e-15
#'
#' @examples
#'
#' vals_to_round = c(NA,-3.5:3.5,NA)
#' # [1]   NA -3.5 -2.5 -1.5 -0.5  0.5  1.5  2.5  3.5   NA
#'
#' # round() will round to even numbers when tied at X.5
#' round(vals_to_round)
#' # [1] NA -4 -2 -2  0  0  2  2  4 NA
#'
#' # round_away_0() will round away from 0 when tied at X.5
#' round_away_0(vals_to_round)
#' # [1] NA -4 -3 -2 -1  1  2  3  4 NA
#'
#' @export

round_away_0 <- function(x, rounding_digits = 0, verbose = FALSE){
  .check_numeric_input(x)
  .check_numeric_input(rounding_digits, lower_bound = 0, upper_bound = 14, scalar = TRUE)

  sign(x) * round(abs(x) + 1e-15, rounding_digits)
}


#' Continuous Variable Compared to Binary Variable Test (VISC)
#'
#' Either Wilcox or T-Test Performed, for unpaired or paired data
#'
#' @param x numeric vector (can include NA values).
#' @param y vector with only 2 levels (can include NA values unless \code{paired = TRUE}).
#' @param method what test to run (wilcox or t-test).
#' @param paired a logical indicating whether you want a paired test.
#' @param verbose a logical variable indicating if warnings and messages should be displayed.
#' @param ... parameters to pass to wilcox_test or t.test functions. For example the testing direction (\code{alternative}) in either call or the \code{var.equal} in the t.test function.
#' @return p-value for comparing x at the different levels of y.
#' @details
#'
#' Runs \code{wilcox_test()} in the coin package, with "exact" distribution and mid-ranks ties method.
#'
#' For one sided tests if \code{y} is a factor variable the level order is respected, otherwise the levels will set to alphabetical order (i.e. if \code{alternative = less} then testing a < b ).
#'
#' If \code{paired = TRUE} assumes the first observations of the first group matches the first observation of the second group, and so on. Also if \code{paired = TRUE} then \code{y} must have the same number of samples for each level.
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

  # Removing cases where x and y are both NA and returning p-value where no complete cases or only one distinct value
  rm_na_and_check_output <- .rm_na_and_check(x, y, y_type = 'binary', verbose = verbose)
  if (is.data.frame(rm_na_and_check_output)) data_here <- rm_na_and_check_output else return(rm_na_and_check_output)

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
