#' Rounds and combines up to three numbers into table friendly presentation
#'
#' stat_paste() takes a up to 3 numeric values, rounds them to a specified digit amount,
#' and then combines then accordingly.
#'
#' @param stat1 first statistic to be pasted.
#' @param stat2 second statistic to be pasted (optional).
#' @param stat3 third statistic to be pasted (optional).
#' @param digits positive integer of length 1 between 0 (default) and 14, giving the amount of digits to round to.
#' @param bound_char the character to be used between stat1 and stat2/stat3. Available options are '(' (default), '[', '{', and '|'.
#' @param sep the string to be used between stat2 and stat3. The default is ', '
#' @param na_str_out the character to replace missing values with.
#' @return string of combined values
#'
#' @details
#'
#' Will take a set of numbers and round them and then combine them in typical table display formats. Will also
#' convert missing estimates to a character string.
#'
#' If one value provided just returns a rounded value or the missing character.
#'
#' For two values it returns \code{stat1 (stat2)}. e.g., mean (sd)
#'
#' For three values it returns \code{stat1 (stat2, stat3)}. e.g., estimate (95\% lower, 95\% upper) or median [min, max]
#'
#' Currently the method does not take in string variables (so doesn't work with pretty_pvalues). Also, does not convert missing
#' values for the lower or upper arguments.
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
#' stat_paste(c(rep(5,5),NA),c(1:5,NA),c(1,NA,2,NA,3,NA),'[')
#'
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





