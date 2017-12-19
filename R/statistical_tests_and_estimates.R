#' Continuous Variable Compared to Binary Variable Test (VISC)
#'
#' Either Wilcox or T-Test Performed, for unpaired or paired data
#'
#' @param x numeric vector (can include NA values)
#' @param y vector with only 2 levels (can include NA values)
#' @param method what test to run (wilcox or t test)
#' @param paired a logical indicating whether you want a paired test.
#' @param verbose a logical variable indicating if warnings and messages should be displayed
#' @param ... parameters to pass to wilcox_test or t.test functions. For example the testing direction (\code{alternative}) in either call or the \code{var.equal} in the t.test function.
#' @return pvalue for comparing x at the different levels of y
#' @details
#'
#' Runs wilcox_test() in the coin package, with "exact" distribution and mid-ranks ties method.
#'
#' For one sided tests if y is a factor variable the level order is respected, otherwise the levels will set to alphabetical order (i.e. if alternative = less then testing a < b )
#'
#' @examples
#'
#' set.seed(5432322)
#' x <- c(rnorm(10,0,3), rnorm(10,3,3))
#' y <- c(rep('a', 10), rep('b', 10))
#' continuous_vs_binary_test(x,y)
#'
#' @export


continuous_vs_binary_test <- function(x, y, method = c('wilcox', 't'), paired = FALSE, verbose = FALSE, ...){
  # Input checking
  method <- match.arg(method)
  .check_numeric_input(x)
  .check_binary_input(y)
  y <- droplevels(factor(y))

  data_here <- na.omit(data.frame(x,y))

  if (length(unique(data_here$x)) == 1) {
    if (verbose) message('"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')
    return(1)
  }

  if (length(unique(data_here$y)) == 1) {
    if (verbose) message('"y" only has 1 level when considering non-missing values of x, so p=NA returned')
    return(NA)
  }

  if (method == 'wilcox') {
    if (paired) {
      as.double(coin::pvalue(coin::wilcoxsign_test(x[data_here$y == levels(data_here$y)[1]] ~ x[data_here$y == levels(data_here$y)[2]], distribution = "exact", zero.method = "Pratt", ...)))
    } else {
      as.double(coin::pvalue(coin::wilcox_test(x ~ y, data = data_here, distribution = "exact", ties.method = "mid-ranks", ...)))
    }
  } else {
    #If both groups have only one distinct value t.test will throw error
    if (any(by(data_here$x, data_here$y, function(xx) {length(unique(xx)) > 1}))) {
      as.double(t.test(data_here$x ~ data_here$y, paired = paired, ...)$p.value)
    } else {
      if (verbose) message('t.test can not run when both levels of "y" have only 1 unique "x" value, so p=NA returned')
      NA
    }
  }
}
