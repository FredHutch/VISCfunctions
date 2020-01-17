#' Rounding Using Round Away From 0 Method
#'
#' round_away_0 takes a numeric vector, rounds them to a specified digit amount using the round away from 0 method for ties (i.e. 1.5). This is the SAS method for rounding.
#'
#' @param x numeric vector (can include NA values).
#' @param digits positive integer of length 1 between 0 (default) and 14, giving the amount of digits to round to.
#' @param trailing_zeros logical indicating if trailing zeros should included (i.e. 0.100 instead of 0.1). Note is set to TRUE output is a character vector
#' @return if \code{trailing_zeros = TRUE} returns a character vector of rounded values with trailing zeros, otherwise returns a numeric vector of rounded values.
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
#' # Can force trailing zeros (will output character vector)
#' round_away_0(vals_to_round, digits = 2, trailing_zeros = TRUE)
#'
#' @export

round_away_0 <- function(x, digits = 0, trailing_zeros = FALSE){
  .check_numeric_input(x, allow_NA = TRUE)
  .check_numeric_input(digits, lower_bound = 0, upper_bound = 14, scalar = TRUE, whole_num = TRUE)

  rounded_vals <- sign(x) * round(abs(x) + 1e-15, digits)

  if (trailing_zeros) {
    # Need to exclude NAs when doing formatting
    rounded_vals[!is.na(rounded_vals)] <- formatC(rounded_vals[!is.na(rounded_vals)], digits, format = 'f')

    #Need to change -0.00... to 0.00...
    neg_to_change <- paste0('-0.',paste0(rep(0,digits), collapse = ''))
    if (any(rounded_vals == neg_to_change, na.rm = TRUE)) rounded_vals[rounded_vals == neg_to_change] <- substr(neg_to_change, 2, nchar(neg_to_change))
  }
  rounded_vals
}

#' Wrapper for round_away_0 to account for non-numeric values
#'
#' Internal wrapper for round_away_0
#'
#' @noRd
#'
#' @param x vector (can include NA values).
#' @param digits positive integer of length 1 between 0 and 14, giving the amount of digits to round to.
#' @param trailing_zeros logical indicating if trailing zeros should included (i.e. 0.100 instead of 0.1). Note is set to TRUE output is a character vector
#' @return if \code{x} non-numeric vector returns x, else if \code{trailing_zeros = TRUE} returns a character vector of rounded values with trailing zeros, otherwise returns a numeric vector of rounded values.
#' @details
#'
#' \code{round_away_0} is not designed for use at precision levels <= 1e-15
#'
#' @examples
#'
#' VISCfunctions:::.round_if_numeric(c(NA,-3.5:3.5,NA))
#' VISCfunctions:::.round_if_numeric(c(NA,letters[1:5],NA))
#'
#'

.round_if_numeric <- function(x, digits = 0, trailing_zeros = FALSE){
  if (is.numeric(x)) round_away_0(x, digits = digits, trailing_zeros = trailing_zeros) else x
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
#' outcome <- c(rnorm(10,0,3), rnorm(10,3,3))
#' grp <- c(rep('a', 10), rep('b', 10))
#' two_samp_cont_test(x = outcome, y = grp, method = 'wilcox', paired = FALSE)
#' two_samp_cont_test(x = outcome, y = grp, method = 'wilcox', paired = TRUE)
#' two_samp_cont_test(x = outcome, y = grp, method = 't', paired = FALSE)
#' two_samp_cont_test(x = outcome, y = grp, method = 't', paired = TRUE)
#'
#' @export


two_samp_cont_test <- function(x, y, method = c('wilcox', 't.test'), paired = FALSE, verbose = FALSE, ...){
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
      as.double(stats::t.test(data_here$x[data_here$y == levels(data_here$y)[1]], data_here$x[data_here$y == levels(data_here$y)[2]], data = data_here, paired = paired, ...)$p.value)
    } else {
      if (verbose) message('t.test can not run when both levels of "y" have only 1 unique "x" value, so p=NA returned')
      NA
    }
  }
}





#' Binary (Response) Variable Compared to Binary (Group) Variable Test (VISC)
#'
#' Either Barnard, Fisher's, or Chi-sq test performed for unpaired data and
#'   McNemar's test for paired data
#'
#' @param x  vector with only 2 levels (can include NA values).
#' @param y vector with only 2 levels (can include NA values unless
#'   \code{method = 'mcnemar'}).
#' @param method what test to run, "barnard" (default), "fisher" ,
#'   "chi.sq" , or "mcnemar")
#' @param barnard_method 	indicates the Barnard method for finding tables as or
#'   more extreme than the observed table: must be either "z-pooled",
#'  "z-unpooled", "santner and snell", "boschloo", "csm", "csm approximate", or
#'  "csm modified". Only used when \code{method = 'barnard'}
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less". You can specify
#'   just the initial letter. Only "two.sided" available for
#'   \code{method = 'chi.sq' or 'mcnemar'}
#' @param verbose a logical variable indicating if warnings and messages should
#'   be displayed.
#' @param ... other parameters to pass to Exact::exact.test when running
#'   Barnard test
#' @return p-value for comparing x at the different levels of y.
#' @details
#'
#'
#' For one sided tests if \code{y} is a factor variable the level order is
#' respected, otherwise the levels will set to alphabetical order (i.e. if
#' \code{alternative = less} then testing a < b ).
#'
#' If \code{method = 'mcnemar'} assumes the first observations of the first
#' group matches the first observation of the second group, and so on. Also if
#' \code{method = 'mcnemar'} then \code{y} must have the same number of samples
#' for each level.
#'
#' If only one value of \code{x} than \code{p=1} is returned, however if only one value of \code{y}
#' than \code{p=NA} is returned. This is to match expactations since normally y is a group variable
#' and x is the outcome (i.e. if both group response rates are 0\% or 100\% we want \code{p=1}
#' returned)
#'
#' @examples
#'
#' set.seed(5432322)
#' outcome <- c(sample(0:1,10,replace = TRUE, prob = c(.75,.25)),
#'              sample(0:1,10,replace = TRUE, prob = c(.25,.75)))
#' grp <- c(rep('a', 10), rep('b', 10))
#' two_samp_bin_test(outcome, grp, method = 'barnard')
#' two_samp_bin_test(outcome, grp, 'fisher')
#' two_samp_bin_test(outcome, grp, 'chi.sq')
#' two_samp_bin_test(outcome, grp, 'mcnemar')
#'
#' @export


two_samp_bin_test <- function(x, y, method = c('barnard', 'fisher' ,'chi.sq' , 'mcnemar'),
                              barnard_method = c("z-pooled", "z-unpooled", "boschloo",
                                                 "santner and snell", "csm",
                                                 "csm approximate", "csm modified"),
                              alternative = c("two.sided", "less", "greater"),
                              verbose = FALSE, ...){
  method <- match.arg(method)
  barnard_method <- match.arg(barnard_method)
  alternative <- match.arg(alternative)
  if (method == 'chi.sq' & alternative != 'two.sided')
    stop('When "method" is chi.sq then "alternative" must be two.sided')
  if (method == 'mcnemar' & alternative != 'two.sided')
    stop('When "method" is mcnemar then "alternative" must be two.sided')
  .check_binary_input(x)
  x <- droplevels(factor(x))
  .check_binary_input(y, paired = ifelse(method == 'mcnemar', TRUE, FALSE))
  y <- droplevels(factor(y))

  # Removing cases where x and y are both NA and returning p-value where no complete cases or only one distinct value
  rm_na_and_check_output <-
    .rm_na_and_check(x, y,
                     x_type = 'fixed_binary', y_type = 'binary', verbose = verbose)
  if (is.data.frame(rm_na_and_check_output))
    data_here <- rm_na_and_check_output else
      return(rm_na_and_check_output)

  if (method == 'barnard') {
    # table needs to have grp variable (y) first
    pval_out <- as.double(
      Exact::exact.test(table(data_here[, c('y', 'x')]),
                        method = barnard_method, to.plot = FALSE,
                        alternative = alternative, ...)$p.value)
  }
  if (method == 'fisher') {
    pval_out <- as.double(
      # Wrapping fisher's test in a pmin function to prevent machine error from
      # giving values greater than 1
      pmin(stats::fisher.test(data_here$x,
                              data_here$y,
                              alternative = alternative)$p.value, 1)
      )
  }
  if (method == 'chi.sq') {
    pval_out <- as.double(
      stats::chisq.test(data_here$x, data_here$y)$p.value
      )
  }
  if (method == 'mcnemar') {
    if (length(unique(stats::na.omit(data_here)$x)) == 2)
        pval_out <- as.double(
          stats::mcnemar.test(data_here$x[which(data_here$y == levels(data_here$y)[1])],
                              data_here$x[which(data_here$y == levels(data_here$y)[2])])$p.value
        ) else
          # setting p to 1 when only 1 level of x (i.e. all 0 or all 1)
          pval_out <- 1

  }
  pval_out
}

#' Wilson Confidence Interval
#'
#' @param x vector of type integer (0/1) or logical (TRUE/FALSE)
#' @param conf.level confidence level (between 0 and 1)
#'
#' @return data.frame with with mean (`mean`), and bounds of confidence interval (`lower`, `upper`)
#' @examples
#'
#' x <- c(rep(0, 500), rep(1, 500))
#' wilson_ci(x, conf.level = .90)
#'
#' @export
wilson_ci <- function(x, conf.level = .95){

  .check_response_input(x)
  .check_numeric_input(conf.level, lower_bound = 0, upper_bound = 1 - 1E-12,
                       scalar = TRUE, whole_num = FALSE, allow_NA = FALSE)

  x <- stats::na.omit(x)

  npos <- sum(x);
  n <- length(x);

  z <- abs(stats::qnorm(1 - (1 - conf.level )/2))
  p <- npos/n
  denom <- 1 + z*z/n
  t1 <- p + z*z/2/n
  t2 <- z * sqrt(p*(1 - p)/n + z*z/4/n/n)
  data.frame(mean = p, lower = (t1 - t2)/denom, upper = (t1 + t2)/denom)

}

