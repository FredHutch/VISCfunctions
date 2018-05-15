#' Rounds and combines up to three numbers into table friendly presentation
#'
#' Takes in up to 3 numeric values, rounds each to a specified digit amount (if numeric), and then combines them accordingly.
#'
#' @param stat1 first statistic to be pasted.
#' @param stat2 second statistic to be pasted (optional).
#' @param stat3 third statistic to be pasted (optional).
#' @param digits positive integer of length 1 between 0 (default) and 14, giving the amount of digits to round to.
#' @param bound_char the character to be used between stat1 and stat2/stat3. Available options are '(' (default), '[', '\{', and '|'.
#' @param sep the string to be used between stat2 and stat3. The default is ', '.
#' @param na_str_out the character to replace missing values with.
#' @return string of combined values
#'
#' @details
#'
#' One value provided - returns a rounded value or the missing character.
#' Two values - returns stat1 (stat2). e.g., mean (sd)
#' Three values - returns stat1 (stat2, stat3). e.g., estimate (95\% lower, 95\% upper) or median [min, max]
#'
#' Currently the method does work with string variables, but of course rounding would not be relevant for strings.
#'
#'
#' @examples
#'
#' stat_paste(5.109293)
#' stat_paste(NA)
#' stat_paste(5.109293, 2.145)
#' stat_paste(5.109293, 1.7645, 8.0345)
#' stat_paste(NA, NA, NA)
#' stat_paste(5.109, "p < 0.001", digits = 3)
#' stat_paste(c(rep(5,5),NA),c(1:5,NA),c(1,NA,2,NA,3,NA),bound_char = '[')
#'
#' library(VISCfunctions.data)
#' library(data.table)
#' data(testData_BAMA)
#' testData_BAMA [, .(
#'   median_min_max = stat_paste(
#'      median(magnitude, na.rm = TRUE),
#'      min(magnitude, na.rm = TRUE),
#'      max(magnitude, na.rm = TRUE)
#'      )), by = .(antigen, visit, group)]
#'
#' @export
stat_paste = function(stat1, stat2 = NULL, stat3 = NULL, digits = 0, bound_char = c('(','[','{','|'), sep = ', ', na_str_out = "---"){
  bound_char <- match.arg(bound_char)
  end_bound_char <-   switch(bound_char,
                      `(` = ')',
                      `[` = ']',
                      `{` = '}',
                      `|` = '|'
  )

  stat1_pasted_obj <-  ifelse(is.na(stat1), na_str_out, as.character(.round_if_numeric(stat1, digits)))
  if (is.null(stat2)) {
    pasted_output <- stat1_pasted_obj
  } else {
    stat2_pasted_obj <-  ifelse(is.na(stat2), na_str_out, as.character(.round_if_numeric(stat2, digits)))
    if (is.null(stat3)) {
      pasted_output <- ifelse(is.na(stat1) & is.na(stat2), na_str_out, paste0(stat1_pasted_obj, " ", bound_char, stat2_pasted_obj, end_bound_char))
    } else {
      stat3_pasted_obj <-  ifelse(is.na(stat3), na_str_out, as.character(.round_if_numeric(stat3, digits)))
      pasted_output <- ifelse(is.na(stat1) & is.na(stat2) & is.na(stat3), na_str_out, paste0(stat1_pasted_obj, " ", bound_char, stat2_pasted_obj, sep, stat3_pasted_obj, end_bound_char))
    }
  }
  pasted_output
}


#' Get SAS-Like Formatted Numbers
#'
#' Formats numbers with trailing zeros when needed (i.e 0.100 if rounding to three digits)
#' @param x numeric vector (can include NA values)
#' @param digits positive integer of length 1 between 1 and 11, giving the amount of digits to format to
#' @return vector a character vector of formated values
#' @details
#'
#' Uses the formatC function, with extra input checking and features. Takes advantage of round_away_from_0 function to ensure expected rounding method of round away from 0 is being used.
#'
#' @examples
#' vals_to_format = c(NA,runif(5),.1,.01,.001,.0005)
#' formatC_VISC(vals_to_format, 3)
#'


formatC_VISC <- function(x, digits = 1, format_in = 'f'){
  if (!is.numeric(x)) {
    stop('x must be a numeric vector')
  }
  if (digits < 0 | digits > 11) {
    stop('digits must between 1 and 11')
  }
  
  formatted_numbers <- formatC(round_away_0(x, digits = digits), digits = digits, format = format_in)
  formatted_numbers[is.na(x)] <- NA
  
  #Need to change -0.0 to 0.0
  neg_to_change <- paste0('-0.',paste0(rep(0,digits), collapse = ''))
  if (any(formatted_numbers == neg_to_change, na.rm = TRUE)) formatted_numbers[formatted_numbers == neg_to_change] <- substr(neg_to_change, 2, nchar(neg_to_change))
  
  as.numeric(formatted_numbers)
}


#' Round and highlight p-values
#'
#' pretty_pvalues() takes a vector of p-values, rounds them to a specified digit amount,
#' allows emphasis when below defined significance level, and returns a character for missing.
#'
#' @param pvalues numeric vector of raw p-values
#' @param digits number of digits to round to
#' @param bold TRUE or FALSE: set to TRUE to bold p-values < the defined significance level
#' @param italic TRUE or FALSE: set to TRUE to italicize p-values < the defined significance level
#' @param background highlight color for p-values < the defined significance level. Default = NULL (no highlighting)
#' @param sig_alpha the defined significance level. Default = 0.05
#' @param missing_char character string that will replace missing values from the p-value vector. Default = "---"
#' @param include_p TRUE or FALSE: set to TRUE to print "p = " before each p-value
#' @param trailing_zeros TRUE or FALSE: default = TRUE, p-values are formatted with trailing zeros to the defined number of digits (i.e. 0.100 instead of 0.1 if digits = 3)
#' @return vector of transformed p-values for table output
#' @examples
#' pvalue_example = c(1, 0.06, 0.0005, NA, 1e-6)
#'
#' pretty_pvalues(pvalue_example, background = "pink")
#'
#' pretty_pvalues(pvalue_example, digits = 4, missing_char = "missing")
#'
#' @export


pretty_pvalues = function(pvalues, digits = 3, bold = FALSE, italic = FALSE, background = NULL, sig_alpha = 0.05, missing_char = '---', include_p = FALSE, trailing_zeros = TRUE){
  
  .check_numeric_input(pvalues)
  .check_numeric_input(sig_alpha)
  .check_numeric_input(digits, lower_bound = 1, upper_bound = 14, scalar = TRUE, whole_num = TRUE)
  
  #Need to set options for no scientific notation, but set back to user preference on exit
  op <- options()
  options(scipen = 10)
  on.exit(options(op))
  
  lower_cutoff = 10^(-digits)
  
  ## relevant p-value indices for specific assignments
  missing_p = which(is.na(pvalues))
  below_cutoff_p = which(pvalues <= lower_cutoff)
  above_cutoff_p = which(pvalues > lower_cutoff)
  sig_p = which(pvalues <= sig_alpha)
  
  
  if (trailing_zeros) pvalues_new = round_away_0(pvalues, trailing_zeros = T, digits = digits) else pvalues_new = as.character(round_away_0(pvalues, digits))
  
  ## manipulate and assign pvalues as characters to output pvalue vector
  #pvalues[above_cutoff_p] = round(pvalue_new[above_cutoff_p], digits)
  pvalues_new[missing_p] = missing_char
  pvalues_new[below_cutoff_p] = paste0("<", lower_cutoff)
  
  # the letter 'p' in front of values
  if (include_p) {
    pvalues_new[below_cutoff_p] = paste0('p',  pvalues_new[below_cutoff_p])
    pvalues_new[above_cutoff_p] = paste0('p=',  pvalues_new[above_cutoff_p])
  }
  
  # formatting
  if (bold == TRUE | italic == TRUE | !is.null(background)) pvalues_new[sig_p] = kableExtra::cell_spec(pvalues_new[sig_p], format = "latex", bold = bold, italic = italic, background = background, escape = FALSE)
  
  # error checking
  if (any(is.na(pvalues_new))) stop("Error in vector assignment.")
  
  pvalues_new
}


