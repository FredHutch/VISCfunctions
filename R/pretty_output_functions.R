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


#' Round and highlight (in Latex) p-values
#'
#' pretty_pvalues() takes a vector of p-values, rounds them to a specified digit amount,
#' cuts off at p <0.001 and highlights when below significance level, returns a character for missing.
#'
#' @param pvalues vector of p-values
#' @param digits amount of digits to round to
#' @param emphasis a character vector specifying the empasis to add, if any, (highlighting, bolding, and italicize), must be one of "none" (default), "highlight", "bold" or "italicize". You can specify just the initial letters.
#' @param sig_alpha the significance level to highlight values for
#' @param missing_char the character to replace missing values with
#' @param include_p TRUE or FALSE. should "p =" be printed in front of p values
#' @param formatted_p TRUE or FALSE. should p values be formatted (i.e. 0.100 instead of 0.1)
#' @return vector of transformed p-values for table output
#' @examples
#' pvalue_example = c(0.5, 0.06, 0.0005, NA, 1e-6)
#'
#' pretty_pvalues(pvalue_example)
#'
#' pretty_pvalues(pvalue_example, digits = 4, missing_char = "")
#'
#' @export


pretty_pvalues = function(pvalues, digits = 3, bold = FALSE, italic = FALSE, background = NULL, sig_alpha = 0.05, missing_char = '---', include_p = FALSE, trailing_zeros = TRUE){
  # 
  # emphasis <- match.arg(emphasis, several.ok = TRUE)
  # #if user didn't specify emphasis (meaning all four options listed) we want to set to 'none'
  # if (length(emphasis) == 4) emphasis = 'none'
  # 
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
  
  
  if (formatted_p) pvalue_new = round_away_0(pvalues, digits) else pvalue_new = as.character(round_away_0(pvalues, digits))
  
  ## manipulate and assign pvalues as characters to output pvalue vector
  #pvalues[above_cutoff_p] = round(pvalue_new[above_cutoff_p], digits)
  pvalues[missing_p] = missing_char
  pvalues[below_cutoff_p] = paste0("<", lower_cutoff)
  
  # the letter 'p' in front of values
  if (include_p) {
    pvalues[below_cutoff_p] = paste0('p',  pvalue_new[below_cutoff_p])
    pvalues[above_cutoff_p] = paste0('p=',  pvalue_new[above_cutoff_p])
  }
  
  # formatting
  if (bold == TRUE | italic == TRUE | !is.null(background)) pvalue_new[sig_p] = kableExtra::cell_spec(pvalue_new[sig_p], format = "latex", bold = bold, italic = italic, background = background)
  
  # error checking
  if (any(is.na(pvalue_new))) stop("Error in vector assignment.")
  
  pvalue_new
}


