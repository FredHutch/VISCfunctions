#' Pasting Together Information for Two Groups
#'
#' Paste together information, often statistics, from two groups. Two predefined pasting exist, mean(sd) and median[min,max], but user may also paste any single measure together.
#'
#'
#' @param data input dataset. User must use consistant naming throughout, with a underscore to seperate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be columns defining the group names (i.e. 'Group1' and 'Group2'), which are used to form the \code{Comparison} variable.
#' @param vars_to_paste vector of names of measures to paste together. can be the predefined 'median_min_max' or 'mean_sd', or any variable as long as they have matching columns for each group (i.e. Group1_MyMeasure and Group2_MyMeasure). The default is both predefined values, 'median_min_max' and 'mean_sd'.
#' @param first_name name of first group (sting before the '_') . Default is 'Group1'.
#' @param second_name name of second group (sting before the '_'). Default is 'Group2'.
#' @param sep_val value to be pasted between the two measures. Default is ' vs. '.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". Will be used in determining the value to be pasted between the group names.
#' @param digits integer indicating the number of decimal places to round to before pasting for numeric variables. Defualt is 0.
#' @param keep_all logical indicating if all other variables in \code{data} should be returned with the pasted variables
#' @details
#'
#' User must use consistant naming throughout, with a underscore to seperate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be columns defining the group names (i.e. \code{Group1} and \code{Group2}), which are used to form the \code{Comparison} variable.
#'
#' \code{alternative} included as a parameter so the direction can easily be seen in one-sided test. If "two.sided" is selected the value to be pasted between the two group names will be set to \code{sep_val}, where "greater" will use " > " and "less" with use " < " as the pasting value.
#'
#'
#'
#' @return data.frame with all the pasted values requested. Each name will have '_info' at the end of the names (i.e. mean_info, median_info, ...)
#' @examples
#'
#'
#' # devtools::install_github(repo = "VIDD-VISC/VISCfunctions.data", host="https://github.fhcrc.org/api/v3")
#' require(VISCfunctions.data)
#' require(data.table)
#' data(exampleData_BAMA)
#'
#' desriptive_stats_by_group <- exampleData_BAMA[, .(
#' Group1 = unique(group[group == 1]), Group2 = unique(group[group == 2]),
#' Group1_n = length(magnitude[group == 1]), Group2_n = length(magnitude[group == 2]),
#' Group1_mean = mean(magnitude[group == 1]), Group2_mean = mean(magnitude[group == 2]),
#' Group1_sd = sd(magnitude[group == 1]), Group2_sd = sd(magnitude[group == 2]),
#' Group1_median = median(magnitude[group == 1]), Group2_median = median(magnitude[group == 2]),
#' Group1_min = min(magnitude[group == 1]), Group2_min = min(magnitude[group == 2]),
#' Group1_max = max(magnitude[group == 1]), Group2_max = max(magnitude[group == 2])
#' ), by = .(visitno,antigen)]
#
#' tbl_grp_paste(data = desriptive_stats_by_group, vars_to_paste = c('n','median_min_max','mean_sd', 'mean', 'sd', 'median', 'min', 'max'), first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", digits = 0, keep_all = TRUE)
#'
#'
#'
#' @export


tbl_grp_paste <- function(data, vars_to_paste = c('median_min_max','mean_sd'), first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", alternative = c("two.sided", "less", "greater"), digits = 0, keep_all = TRUE){

  #####Checking variables being used

  data <- as.data.frame(data)
  alternative <- match.arg(alternative)
  .check_numeric_input(digits, lower_bound = 0, scalar = TRUE)
  if (sum(first_name == names(data)) != 1) stop('Expecting one column named "', first_name , '" in input dataset, but there are ', sum(first_name == names(data)), ' present')
  if (sum(second_name == names(data)) != 1) stop('Expecting one column named "', second_name , '" in input dataset, but there are ', sum(second_name == names(data)), ' present')

  # Need to define which variables to check. Special considerations for the predefined values
  vars_to_check <- vars_to_paste[!vars_to_paste %in% c('median_min_max','mean_sd')]
  if (any(vars_to_paste == 'median_min_max')) vars_to_check <- unique(c(vars_to_check, 'median', 'min', 'max'))
  if (any(vars_to_paste == 'mean_sd')) vars_to_check <- unique(c(vars_to_check, 'mean', 'sd'))

  # Need to check the group1 and group2 version of each variable being pasted
  group1_vars_to_check <- paste0(first_name, '_', vars_to_check)
  group2_vars_to_check <- paste0(second_name, '_', vars_to_check)
  for (i in 1:length(vars_to_check)) {
    if (sum(group1_vars_to_check[i] == names(data)) != 1) stop('Expecting one column named "', group1_vars_to_check[i] , '" in nput dataset, but there are ', sum(group1_vars_to_check[i] == names(data)), ' present')
    temp_group1_var <- data[, group1_vars_to_check[i] == names(data)]
    .check_numeric_input(temp_group1_var)

    if (sum(group2_vars_to_check[i] == names(data)) != 1) stop('Expecting one column named "', group2_vars_to_check[i] , '" in nput dataset, but there are ', sum(group2_vars_to_check[i] == names(data)), ' present')
    temp_group2_var <- data[, group2_vars_to_check[i] == names(data)]
    .check_numeric_input(temp_group2_var)
  }



  # If user doesn't define first_sep_val it is set to sep_val
  if (is.null(first_sep_val)) first_sep_val <- sep_val


  ##### Pasting variables

  # Comparison variable
  comparison_sep <- switch(alternative,
                            two.sided = sep_val,
                            less = ' < ',
                            greater = ' > ')

  comparison_var <- paste0(data[, first_name == names(data)], comparison_sep, data[, second_name == names(data)])

  # Other variables
  pasted_results <- list()
  for (i in 1:length(vars_to_paste)) {
    if (vars_to_paste[i] == 'median_min_max') {
      pasted_results[[i]] <- paste0(
        paste0(round_if_numeric(data[, paste0(first_name, '_mean')], digits), '[',
               round_if_numeric(data[, paste0(first_name, '_min')], digits), ', ',
               round_if_numeric(data[, paste0(first_name, '_max')], digits), ']', sep = ''),
        sep_val,
        paste0(round_if_numeric(data[, paste0(second_name, '_mean')], digits), '[',
               round_if_numeric(data[, paste0(second_name, '_min')], digits), ', ',
               round_if_numeric(data[, paste0(second_name, '_max')], digits), ']', sep = '')
      )
    } else if (vars_to_paste[i] == 'mean_sd') {
      pasted_results[[i]] <- paste0(
        paste0(round_if_numeric(data[, paste0(first_name, '_mean')], digits), '(',
               round_if_numeric(data[, paste0(first_name, '_sd')], digits), ')', sep = ''),
        sep_val,
        paste0(round_if_numeric(data[, paste0(second_name, '_mean')], digits), '(',
               round_if_numeric(data[, paste0(second_name, '_sd')], digits), ')', sep = '')
      )
    } else {
      pasted_results[[i]] <- paste0(round_if_numeric(data[, paste0(first_name, '_', vars_to_paste[i])], digits),
        sep_val,
        round_if_numeric(data[, paste0(second_name, '_', vars_to_paste[i])], digits)
      )
    }
  }
  names(pasted_results) <- paste0(vars_to_paste, '_info')

  pasted_results <- data.frame('Comparison' = comparison_var,pasted_results)

  # Returning all data if desired
  if (keep_all) {
    index_to_keep <- !names(data) %in% c(first_name, second_name, group1_vars_to_check, group2_vars_to_check)
    data.frame(data[, index_to_keep], pasted_results)
  } else {
    pasted_results
  }

}

# Function to round only if numeric variable
round_if_numeric <- function(xx, digits = 0){
  if (is.numeric(xx)) round_away_0(xx, digits = digits) else xx
}




