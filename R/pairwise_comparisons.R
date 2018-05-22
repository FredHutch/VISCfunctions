#' Pairwise Testing for a Continuous Variable
#'
#' Takes a continuous variable and performs pairwise testing (t-test or wilcox test)
#'
#' @param x numeric vector (can include NA values).
#' @param group categorical vector of group values.
#' @param paired a logical variable indicating whether to do a paired test.
#' @param id vector which contains the id information (so \code{x} values can be linked between groups). Only used and must be present when paired = TRUE.
#' @param method what test to run ("wilcox" or "t" test).
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param sorted_group a vector listing the group testing order from lowest to highest.
#' @param num_needed_for_test required sample size (per group) to perform test. Note at least 2 distinct values per group is always needed for testing.
#' @param digits digits to round for magnitude descriptive statistics (0 is default).
#' @param trailing_zeros logical indicating if trailing zeros should be included in the desriptive statistics (i.e. 0.100 instead of 0.1). Note if set to TRUE output is a character vector.
#' @param sep_val value to be pasted between the two measures. Default is ' vs. '.
#' @param na_str_out the character to replace missing values with.
#' @param verbose a logical variable indicating if warnings and messages should be displayed.
#' @return Returns a data frame with all possible pairwise comparisons. Variables include Comparison, SampleSizes, Median_Min_Max (group stats; median [min, max]), Mean_SD (group stats; mean (sd)), MagnitudeTest (wilcoxon/t p value), PerfectSeperation (a logical flag indicating if there is perfect seperation).
#' @details
#'
#' Runs wilcox_test() in the coin package, with "exact" distribution.
#'
#' If \code{sorted_group} is not specified then testing order based on factor levels if \code{group} is a factor, and alphabetical order otherwise
#'
#' \code{trailing_zeros} does not impact p value column, which will be a numeric column regardless.
#'
#' If \code{paired = TRUE} the descriptive statistics are shown for obsevations that have non-missing values for both groups.
#'
#' @examples
#'
#' x_example = c(NA, sample(1:50, 50), sample(51:99, 49), 1111,2222)
#' group_example = c(rep(1:4,25),'a','a')
#'
#'pairwise_test_cont(x_example,group_example, num_needed_for_test = 2)
#'
#'pairwise_test_cont(
#'x_example,group_example, alternative = "less",
#'   sorted_group = c(1:4, 'a'),num_needed_for_test = 2)
#'
#'
#'
#' # Examples with Real World Data
#' library(VISCfunctions.data)
#' library(data.table)
#' library(dplyr)
#'
#'
#' BAMA Assay Data Example
#' data(exampleData_BAMA)
#'
#' ## Group Comparison
#' # using data.table
#'group_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
#'  x = magnitude, group = group, paired = FALSE, method = 'wilcox',
#'  alternative = 'less', num_needed_for_test = 3, digits = 3,
#'  trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
#'),
#'by = .(antigen, visitno)][order(antigen, visitno)]
#'
#' # using dplyr
#'group_testing_tibble <- exampleData_BAMA %>%
#'    group_by(antigen, visitno) %>%
#'    do(pairwise_test_cont(x = .$magnitude, group = .$group, paired = F, method = 'wilcox', alternative = "less", digits = 3, num_needed_for_test = 3, verbose = TRUE))
#'
#' # Confirming both methods are the same
#' all.equal(group_testing_dt,data.table(group_testing_tibble))
#'
#'
#' ## Timepoint Comparison
#'timepoint_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
#'x = magnitude, group = visitno, paired = TRUE,id = pubID, method = 'wilcox',
#'alternative = 'less', num_needed_for_test = 3, digits = 3,
#'trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
#'),
#'by = .(antigen, group)]
#'
#'
#' ICS Assay Data Example
#' data(exampleData_ICS)
#'
#' ## Group Comparison
#' # using data.table
#'group_testing_dt <- exampleData_ICS[, pairwise_test_cont(
#'  x = PercentCellNet, group = Group, paired = FALSE, method = 'wilcox',
#'  alternative = 'less', num_needed_for_test = 3, digits = 4,
#'  trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
#'),
#'by = .(Stim, Parent, Population, Visit)][order(Stim, Parent, Population, Visit)]
#'
#' # Timepoint Comparison
#'timepoint_testing_dt <- exampleData_ICS[, pairwise_test_cont(
#'x = PercentCellNet, group = Visit, paired = TRUE,id = pubID, method = 'wilcox',
#'alternative = 'less', num_needed_for_test = 3, digits = 4,
#'trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
#'),
#'by = .(Stim, Parent, Population, Group)]
#'
#'

#' @export


pairwise_test_cont <- function(x, group, paired = FALSE, id = NULL, method = c('wilcox', 't.test'), alternative = c("two.sided", "less", "greater"), sorted_group = NULL, num_needed_for_test = 3, digits = 0, trailing_zeros = TRUE, sep_val = " vs. ", na_str_out = "---", verbose = FALSE){
  #Input Checking
  .check_numeric_input(x)
  if (length(x) != length(group)) stop('"x" and "group" must be same length')
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  if (paired & is.null(id))  stop('"id" must be present when "paired" = TRUE')
  if (!is.null(sorted_group) & any(!group %in% c(NA,sorted_group))) stop('"sorted_group" must contain all possible values of "group"')

  #Dropping any missing in either x or group
  if (paired) keep_index <- !is.na(x) & !is.na(group) & !is.na(id) else keep_index <- !is.na(x) & !is.na(group)
  x <- x[keep_index]
  group <- group[keep_index]
  if (paired) id <- id[keep_index]

  #Need to set factor levels so can provide directional one-sided tests if needed
  if (is.null(sorted_group)) group <- droplevels(factor(group)) else group <- droplevels(factor(group, levels = sorted_group[!is.na(match(sorted_group, group))]))
  if (is.null(sorted_group) & verbose) message('"sorted_group" not specified so testing in following order: ', paste0(levels(group), collapse = ', '))

  n_levels <- nlevels(group)
  levels_here <- levels(group)

  if (n_levels == 0) {
    if (verbose) message('No non-missing values, so nothing to compare')
    return(NULL)
  }
  if (n_levels == 1) {
    if (verbose) message('Only one group has any non-missing values, so nothing to compare')
    return(NULL)
  }

  results_list <- list()
  for (i in 1:(n_levels - 1)) {
    for (j in (i + 1):n_levels) {
      i_group <- levels_here[i]
      j_group <- levels_here[j]

      if (paired) {

        #For paired testing only using non-missing values in both groups
        i_data <- data.frame(x = x[group == i_group], id = id[group == i_group])
        j_data <- data.frame(y = x[group == j_group], id = id[group == j_group])
        data_here <- na.omit(merge(i_data, j_data, by = 'id'))
        i_vals <- data_here$x
        j_vals <- data_here$y
        vals_here <-  c(i_vals, j_vals)
        groups_here <-  c(rep(i_group, nrow(data_here)), rep(j_group, nrow(data_here)))

      } else {

        i_vals <- x[group == i_group]
        j_vals <- x[group == j_group]
        vals_here <-  x[group  %in% c(i_group, j_group)]
        groups_here <-  factor(group[group  %in% c(i_group, j_group)], levels = c(i_group, j_group))

      }


      stats_by_group <- data.frame(Group1 = i_group, Group2 = j_group,
                                   Group1_n = sum(!is.na(i_vals)), Group2_n = sum(!is.na(j_vals)),
                                   Group1_min = min(i_vals, na.rm = T), Group2_min = min(j_vals, na.rm = T),
                                   Group1_median = median(i_vals, na.rm = T), Group2_median = median(j_vals, na.rm = T),
                                   Group1_max = max(i_vals, na.rm = T), Group2_max = max(j_vals, na.rm = T),
                                   Group1_mean = mean(i_vals, na.rm = T), Group2_mean = mean(j_vals, na.rm = T),
                                   Group1_sd = sd(i_vals, na.rm = T), Group2_sd = sd(j_vals, na.rm = T)
                                   )

      #Getting perfect seperation flag
      if (length(unique(groups_here)) == 2) {
        i_group_range <- range(i_vals, na.rm = T)
        y_group_range <- range(j_vals, na.rm = T)
        if ((i_group_range[1] >  y_group_range[2]) | (y_group_range[1] >  i_group_range[2])) perfect_seperation <- TRUE else perfect_seperation <- FALSE
      } else {
        perfect_seperation <- NA
      }

      #All conditions needed for wilcoxon test to run
      if ((length(i_vals) >= num_needed_for_test) & (length(j_vals) >= num_needed_for_test) &
          (length(unique(groups_here)) == 2)) {

        mag_p <- two_samp_cont_test(x = vals_here, y = groups_here, paired = paired, method = method, alternative = alternative, verbose = verbose)

      } else {
        if (verbose) message(paste0('x does not have at least ', num_needed_for_test,' non missing per group, so no test run (MagnitudeTest=NA returned)'))
        mag_p <- NA_real_
      }

      results_list[[length(results_list) + 1]] <- data.frame(stats_by_group, `MagnitudeTest` = mag_p, PerfectSeperation = perfect_seperation, stringsAsFactors = FALSE)
    }
  }

  restuls <- do.call(base::rbind, results_list)

  # Pasting together stats
  pasted_results <- paste_tbl_grp(data = restuls, vars_to_paste = c("n","median_min_max","mean_sd"), first_name = 'Group1', second_name = 'Group2', sep_val = sep_val, alternative = alternative, digits = digits, trailing_zeros = trailing_zeros, keep_all = TRUE, verbose = verbose)


  # If paired need to return number of pairs for the sample size (note Group1_n = Group2_n in paired cases)
  if (paired) pasted_results$n_comparison <- restuls$Group1_n

  data.frame(Comparison = pasted_results$Comparison,
             SampleSizes = pasted_results$n_comparison,
             Median_Min_Max = pasted_results$median_min_max_comparison,
             Mean_SD = pasted_results$mean_sd_comparison,
             MagnitudeTest = restuls$MagnitudeTest,
             PerfectSeperation = restuls$PerfectSeperation,
             stringsAsFactors = FALSE)

}


