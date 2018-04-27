#' Pasting Together Information for Two Groups
#'
#' Paste together information, often statistics, from two groups. Two predefined pasting exist, mean(sd) and median[min,max], but user may also paste any single measure together.
#'
#'
#' @param data input dataset. User must use consistant naming throughout, with a underscore to seperate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be columns defining the group names (i.e. 'Group1' and 'Group2'), which are used to form the \code{Comparison} variable.
#' @param vars_to_paste vector of names of measures to paste together. Can be the predefined 'median_min_max' or 'mean_sd', or any variable as long as they have matching columns for each group (i.e. Group1_MyMeasure and Group2_MyMeasure). Multiple mesures can be requested. Also can specify "all" (default), which will run 'median_min_max' and 'mean_sd', as well as any pairs of columns in the proper format.
#' @param first_name name of first group (sting before the '_') . Default is 'Group1'.
#' @param second_name name of second group (sting before the '_'). Default is 'Group2'.
#' @param sep_val value to be pasted between the two measures. Default is ' vs. '.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". Will be used in determining the value to be pasted between the group names.
#' @param digits integer indicating the number of decimal places to round to before pasting for numeric variables. Default is 0.
#' @param keep_all logical indicating if all other variables in \code{data} should be returned with the pasted variables
#' @param verbose a logical variable indicating if warnings and messages should be displayed.
#' @details
#'
#' User must use consistant naming throughout, with a underscore to seperate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be columns defining the group names (i.e. \code{Group1} and \code{Group2}), which are used to form the \code{Comparison} variable.
#'
#' \code{alternative} included as a parameter so the direction can easily be seen in one-sided test. If "two.sided" is selected the value to be pasted between the two group names will be set to \code{sep_val}, where "greater" will use " > " and "less" with use " < " as the pasting value.
#'
#'
#'
#' @return data.frame with all the pasted values requested. Each name will have '_comparison' at the end of the names (i.e. mean_comparison, median_comparison, ...)
#' @examples
#'
#'
#' # devtools::install_github(repo = "VIDD-VISC/VISCfunctions.data", host="https://github.fhcrc.org/api/v3")
#' require(VISCfunctions.data)
#' data(exampleData_BAMA)
#'
#' desriptive_stats_by_group <- exampleData_BAMA[, .(
#'      Group1 = unique(group[group == 1]), Group2 = unique(group[group == 2]),
#'      Group1_n = length(magnitude[group == 1]), Group2_n = length(magnitude[group == 2]),
#'      Group1_mean = mean(magnitude[group == 1]), Group2_mean = mean(magnitude[group == 2]),
#'      Group1_sd = sd(magnitude[group == 1]), Group2_sd = sd(magnitude[group == 2]),
#'      Group1_median = median(magnitude[group == 1]), Group2_median = median(magnitude[group == 2]),
#'      Group1_min = min(magnitude[group == 1]), Group2_min = min(magnitude[group == 2]),
#'      Group1_max = max(magnitude[group == 1]), Group2_max = max(magnitude[group == 2])
#' ), by = .(visitno,antigen)]
#'
#'
#'
#' tbl_grp_paste(data = desriptive_stats_by_group, vars_to_paste = 'all', first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", digits = 0, keep_all = TRUE)
#'
#' # Only getting predefined values
#' tbl_grp_paste(data = desriptive_stats_by_group, vars_to_paste = c('median_min_max','mean_sd'), first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", digits = 0, keep_all = TRUE)
#' # Playing around with options
#' tbl_grp_paste(data = desriptive_stats_by_group, vars_to_paste = 'all', first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", alternative = 'less', digits = 5, keep_all = FALSE)
#'
#'
#' @import data.table
#' @export


tbl_grp_paste <- function(data, vars_to_paste = 'all', first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", alternative = c("two.sided", "less", "greater"), digits = 0, keep_all = TRUE, verbose = FALSE){

  #####Checking variables being used

  data_here <- as.data.frame(data)
  alternative <- match.arg(alternative)
  .check_numeric_input(digits, lower_bound = 0, scalar = TRUE)
  if (sum(first_name == names(data_here)) != 1) stop('Expecting one column named "', first_name , '" in input dataset, but there are ', sum(first_name == names(data_here)), ' present')
  if (sum(second_name == names(data_here)) != 1) stop('Expecting one column named "', second_name , '" in input dataset, but there are ', sum(second_name == names(data_here)), ' present')
  if (length(vars_to_paste) != 1 & any(vars_to_paste == 'all')) {
    vars_to_paste = 'all'
    if (verbose) message('Since "all" specified other entries of vars_to_paste ignored.')
  }

  # Defining vars when vars_to_paste set to 'all'
    if (length(vars_to_paste) == 1 & any(vars_to_paste == 'all')) {
      temp_group1_names <- names(data_here)[substr(names(data_here), 0, nchar(first_name)) == first_name]
      temp_group2_names <- names(data_here)[substr(names(data_here), 0, nchar(second_name)) == second_name]
      temp_group1_measures <- gsub(paste0(first_name,'_'), '', temp_group1_names, fixed = T)
      temp_group2_measures <- gsub(paste0(second_name,'_'), '', temp_group2_names, fixed = T)
      vars_to_paste_here <- unique(intersect(temp_group1_measures, temp_group2_measures))

      # Adding speacial cases
      if (sum(vars_to_paste_here %in% c('median','min','max')) == 3) vars_to_paste_here <- c(vars_to_paste_here, 'median_min_max')
      if (sum(vars_to_paste_here %in% c('mean','sd')) == 2) vars_to_paste_here <- c(vars_to_paste_here, 'mean_sd')

      if (verbose) message('The following measures will be combined: ', paste0(vars_to_paste_here, collapse = ', '))
    } else {
      vars_to_paste_here <- unique(vars_to_paste)
    }
    # Giving a message if nothing to return
    if (vars_to_paste == 'all' & length(vars_to_paste_here) == 0) {
      if (verbose) message('"all" specified, but no matching columns to paste')
      if (keep_all) return(data) else return(NULL)
    }

  # Need to define which variables to check. Special considerations for the predefined values
  vars_to_check <- vars_to_paste_here[!vars_to_paste_here %in% c('median_min_max','mean_sd')]
  if (any(vars_to_paste_here == 'median_min_max')) vars_to_check <- unique(c(vars_to_check, 'median', 'min', 'max'))
  if (any(vars_to_paste_here == 'mean_sd')) vars_to_check <- unique(c(vars_to_check, 'mean', 'sd'))

  # Need to check the group1 and group2 version of each variable being pasted
  group1_vars_to_check <- paste0(first_name, '_', vars_to_check)
  group2_vars_to_check <- paste0(second_name, '_', vars_to_check)
  for (i in 1:length(vars_to_check)) {
    if (sum(group1_vars_to_check[i] == names(data_here)) != 1) stop('Expecting one column named "', group1_vars_to_check[i] , '" in input dataset, but there are ', sum(group1_vars_to_check[i] == names(data_here)), ' present')
    temp_group1_var <- data_here[, group1_vars_to_check[i] == names(data_here)]
    if (is.numeric(temp_group1_var)) .check_numeric_input(temp_group1_var)

    if (sum(group2_vars_to_check[i] == names(data_here)) != 1) stop('Expecting one column named "', group2_vars_to_check[i] , '" in input dataset, but there are ', sum(group2_vars_to_check[i] == names(data_here)), ' present')
    temp_group2_var <- data_here[, group2_vars_to_check[i] == names(data_here)]
    if (is.numeric(temp_group2_var)) .check_numeric_input(temp_group2_var)
  }



  ##### Pasting variables

  # Comparison variable
  comparison_sep <- switch(alternative,
                            two.sided = sep_val,
                            less = ' < ',
                            greater = ' > ')

  comparison_var <- paste0(data_here[, first_name == names(data_here)], comparison_sep, data_here[, second_name == names(data_here)])

  # Other variables
  pasted_results <- list()
  for (i in 1:length(vars_to_paste_here)) {
    if (vars_to_paste_here[i] == 'median_min_max') {
      pasted_results[[i]] <- paste0(
        paste0(.round_if_numeric(data_here[, paste0(first_name, '_median')], digits), '[',
               .round_if_numeric(data_here[, paste0(first_name, '_min')], digits), ', ',
               .round_if_numeric(data_here[, paste0(first_name, '_max')], digits), ']', sep = ''),
        sep_val,
        paste0(.round_if_numeric(data_here[, paste0(second_name, '_median')], digits), '[',
               .round_if_numeric(data_here[, paste0(second_name, '_min')], digits), ', ',
               .round_if_numeric(data_here[, paste0(second_name, '_max')], digits), ']', sep = '')
      )
    } else if (vars_to_paste_here[i] == 'mean_sd') {
      pasted_results[[i]] <- paste0(
        paste0(.round_if_numeric(data_here[, paste0(first_name, '_mean')], digits), '(',
               .round_if_numeric(data_here[, paste0(first_name, '_sd')], digits), ')', sep = ''),
        sep_val,
        paste0(.round_if_numeric(data_here[, paste0(second_name, '_mean')], digits), '(',
               .round_if_numeric(data_here[, paste0(second_name, '_sd')], digits), ')', sep = '')
      )
    } else {
      pasted_results[[i]] <- paste0(.round_if_numeric(data_here[, paste0(first_name, '_', vars_to_paste_here[i])], digits),
        sep_val,
        .round_if_numeric(data_here[, paste0(second_name, '_', vars_to_paste_here[i])], digits)
      )
    }
  }
  names(pasted_results) <- paste0(vars_to_paste_here, '_comparison')

  pasted_results <- data.frame('Comparison' = comparison_var,pasted_results, stringsAsFactors = FALSE)

  # Returning all data if desired
  if (keep_all) {
    index_to_keep <- !names(data_here) %in% c(first_name, second_name, group1_vars_to_check, group2_vars_to_check)
    data.frame(data_here[, index_to_keep], pasted_results, stringsAsFactors = FALSE)
  } else {
    pasted_results
  }

}

# Wrapper for round_away_0 only for numeric verianbles
.round_if_numeric <- function(x, digits = 0){
  if (is.numeric(x)) round_away_0(x, digits = digits) else x
}