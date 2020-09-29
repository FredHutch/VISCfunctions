#' Pairwise Testing for a Continuous Variable
#'
#' Takes a continuous variable and performs pairwise testing (t-test or wilcox test)
#'
#' @param x numeric vector (can include NA values).
#' @param group categorical vector of group values.
#' @param paired a logical variable indicating whether to do a paired test.
#' @param id vector which contains the id information (so \code{x} values can be linked between groups). Only used and must be present when paired = TRUE.
#' @param method what test to run ("wilcox" or "t.test").
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param sorted_group a vector listing the group testing order from lowest to highest.
#' @param num_needed_for_test required sample size (per group) to perform test. Note at least 2 distinct values per group are always needed for testing.
#' @param digits digits to round for magnitude descriptive statistics (default = 0).
#' @param trailing_zeros logical indicating if trailing zeros should be included in the descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to TRUE, output is a character vector.
#' @param sep_val value to be pasted between the two measures. Default is ' vs. '.
#' @param na_str_out the character string in the output table that replaces missing values.
#' @param verbose a logical variable indicating if warnings and messages should be displayed.
#' @return Returns a data frame with all possible pairwise comparisons. Variables include Comparison, SampleSizes, Median_Min_Max (group stats; median [min, max]), Mean_SD (group stats; mean (sd)), MagnitudeTest (wilcox/t-test p-value), PerfectSeperation (a logical flag indicating if there is perfect seperation).
#' @details
#'
#' Runs \code{wilcox_test()} in the coin package, with "exact" distribution.
#'
#' If \code{sorted_group} is not specified then testing order based on factor levels if \code{group} is a factor, and alphabetical order otherwise
#'
#' \code{trailing_zeros} does not impact p-value column, which will be a numeric column regardless.
#'
#' If \code{paired = TRUE} the descriptive statistics are shown for observations that have non-missing values for both groups.
#'
#' @examples
#'
#'x_example <- c(NA, sample(1:50, 50), sample(51:99, 49), 1111,2222)
#'group_example <- c(rep(1:4,25),'a','a')
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
#' library(dplyr)
#'
#' # BAMA Assay Data Example
#' data(exampleData_BAMA)
#'
#' ## Group Comparison
#'group_testing_tibble <- exampleData_BAMA %>%
#'    group_by(antigen, visitno) %>%
#'    summarise(pairwise_test_cont(x = magnitude,
#'                                 group = group,
#'                                 paired = FALSE,
#'                                 method = 'wilcox',
#'                                 alternative = "less",
#'                                 sorted_group = c(1,2),
#'                                 digits = 3,
#'                                 num_needed_for_test = 3,
#'                                 verbose = TRUE),
#'             .groups = "keep")
#'
#'
#' ## Timepoint Comparison
#'timepoint_testing_dt <- exampleData_BAMA %>%
#'                        group_by(antigen, group) %>%
#'                        summarise(pairwise_test_cont(x = magnitude,
#'                                                    group = visitno,
#'                                                    paired = TRUE,
#'                                                    id = pubID,
#'                                                    method = 'wilcox',
#'                                                    sorted_group = c(0,1,2),
#'                                                    alternative = 'less',
#'                                                    num_needed_for_test = 3,
#'                                                    digits = 3,
#'                                                    trailing_zeros = TRUE,
#'                                                    sep_val = ' vs. ',
#'                                                    verbose = TRUE),
#'                                  .groups = "keep")
#'
#'
#' # ICS Assay Data Example
#' data(exampleData_ICS)
#'
#' ## Group Comparison
#' # using dplyr
#'exampleData_ICS %>%
#'group_by(Stim, Parent, Population, Visit) %>%
#'summarise(pairwise_test_cont(x = PercentCellNet,
#'                             group = Group,
#'                             paired = FALSE,
#'                             method = 'wilcox',
#'                             alternative = 'less',
#'                             sorted_group = c(1,2,3,4),
#'                             num_needed_for_test = 3,
#'                             digits = 4,
#'                             trailing_zeros = TRUE,
#'                             sep_val = ' vs. ',
#'                             verbose = TRUE),
#'          .groups = "keep")
#'
#' # Timepoint Comparison
#'timepoint_testing_dt <- exampleData_ICS %>%
#'                        group_by(Stim, Parent, Population, Group) %>%
#'                        summarise(pairwise_test_cont(x = PercentCellNet,
#'                                                     group = Visit,
#'                                                     paired = TRUE,
#'                                                     id = pubID,
#'                                                     method = 'wilcox',
#'                                                     sorted_group = c(0,1,2),
#'                                                     alternative = 'less',
#'                                                     num_needed_for_test = 3,
#'                                                     digits = 4,
#'                                                     trailing_zeros = TRUE,
#'                                                     sep_val = ' vs. ',
#'                                                     verbose = TRUE),
#'                        .groups = "keep")
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
  if (is.null(sorted_group) & (alternative != 'two.sided') & verbose) message('"sorted_group" not specified so testing in following order: ', paste0(levels(group), collapse = ', '))

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
        data_here <- stats::na.omit(merge(i_data, j_data, by = 'id'))
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
                                   Group1_median = stats::median(i_vals, na.rm = T), Group2_median = stats::median(j_vals, na.rm = T),
                                   Group1_max = max(i_vals, na.rm = T), Group2_max = max(j_vals, na.rm = T),
                                   Group1_mean = mean(i_vals, na.rm = T), Group2_mean = mean(j_vals, na.rm = T),
                                   Group1_sd = stats::sd(i_vals, na.rm = T), Group2_sd = stats::sd(j_vals, na.rm = T)
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

  results <- do.call(base::rbind, results_list)

  # Pasting together stats
  pasted_results <- paste_tbl_grp(data = results, vars_to_paste = c("n","median_min_max","mean_sd"), first_name = 'Group1', second_name = 'Group2', sep_val = sep_val, alternative = alternative, digits = digits, trailing_zeros = trailing_zeros, keep_all = TRUE, verbose = verbose)


  # If paired need to return number of pairs for the sample size (note Group1_n = Group2_n in paired cases)
  if (paired) pasted_results$n_comparison <- results$Group1_n

  data.frame(Comparison = pasted_results$Comparison,
             SampleSizes = pasted_results$n_comparison,
             Median_Min_Max = pasted_results$median_min_max_comparison,
             Mean_SD = pasted_results$mean_sd_comparison,
             MagnitudeTest = results$MagnitudeTest,
             PerfectSeperation = results$PerfectSeperation,
             stringsAsFactors = FALSE)

}






#' Binary Variable Pairwise Testing
#'
#' Takes a binary variable (e.g., response status) and performs pairwise testing.
#'   Performs either Barnard, Fisher's, or Chi-sq test for unpaired data and
#'   McNemar's test for paired data.
#'
#' @param x numeric vector (0/1) or logical vector or (F/T)
#'   (can include NA values)
#' @param group vector of group values.
#' @param id vector which contains the id information
#'   (so \code{x} values can be linked between groups).
#'   Only used and must be present when \code{method = 'mcnemar'}.
#' @param method what test to run, "barnard" (default), "fisher" ,"chi.sq" ,
#'   or "mcnemar")
#' @param barnard_method 	indicates the Barnard method for finding tables as
#'   or more extreme than the observed table: must be either "Z-pooled",
#'   "Z-unpooled", "Santner and Snell", "Boschloo", "CSM", "CSM approximate",
#'   or "CSM modified". Only used when \code{method = 'barnard'}
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less". You can specify
#'   just the initial letter. Only "two.sided" available for
#'   \code{method = 'chi.sq' or 'mcnemar'}
#' @param sorted_group a vector listing the group testing order from lowest to
#'   highest, if performing one sided tests
#' @param num_needed_for_test required sample size (per group) to perform test.
#' @param conf_level The level of confidence to be used in the confidence
#'   interval.
#' @param digits digits to round for descriptive statistics
#' @param trailing_zeros logical indicating if trailing zeros should be included
#'   in the descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to
#'   TRUE, output is a character vector.
#' @param sep_val value to be pasted between the two measures. Default is
#'   ' vs. '.
#' @param na_str_out the character string in the output table that replaces
#'   missing values.
#' @param verbose a logical variable indicating if warnings and messages should
#'   be displayed
#' @param latex_output will this table be used for latex output (default is FALSE)
#' @param ... other parameters to pass to Exact::exact.test when running
#'   Barnard test
#' @return Returns a data frame with all possible pairwise comparisons.
#'   Variables include Comparison, ResponseStats (group stats; number positive /
#'  number = rate (Wilson CI Bounds)), ResponseTest (fisher/chisq p value),
#'  PerfectSeperation (a logical flag indicating if one group if 0% and the
#'  other 100%)
#' @details
#'
#' If all values of \code{x} are NA, the function will return NULL. This is to allow for nice
#' return when looping through function with dplyr \code{group_by} and \code{group_modify}
#'
#' For one sided tests if \code{sorted_group = NULL} than the factor level order of \code{group}
#' is respected, otherwise the levels will set to alphabetical order (i.e. if
#' \code{alternative = less} then testing a < b ).
#'
#' If planning on using the table in a latex document then set \code{latex_output = TRUE}.
#' This will set the \code{\%} symbol to \code{\\\\\%} in the binary percentages
#'
#' @examples
#'
#' set.seed(1)
#' x_example = c(NA,sample(0:1,50,replace = TRUE, prob = c(.75,.25)),
#'   sample(0:1,50,replace = TRUE, prob = c(.25,.75)),0,0,1,1)
#' group_example = c(rep(1,25),NA,rep(2,25),rep(3,25),rep(4,25),'a','a','b','b')
#'
#'pairwise_test_bin(x_example,group_example, num_needed_for_test = 2)
#'
#'pairwise_test_bin(
#'x_example,group_example, alternative = "less",
#'   sorted_group = c(1:4, 'a','b'),num_needed_for_test = 2)
#'
#' # Examples with Real World Data
#' library(dplyr)
#'
#' # BAMA Assay Data Example
#' data(exampleData_BAMA)
#'
#' ## Group Comparison
#'group_testing <- exampleData_BAMA %>%
#'    group_by(antigen, visitno) %>%
#'    group_modify(~ as.data.frame(
#'        pairwise_test_bin(x = .$response, group = .$group,
#'                method = 'barnard', alternative = 'less',
#'                num_needed_for_test = 3, digits = 1,
#'                trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#'
#'
#' ## Timepoint Comparison
#'timepoint_testing <- exampleData_BAMA %>%
#'    group_by(antigen, group) %>%
#'    group_modify(~ as.data.frame(
#'        pairwise_test_bin(x = .$response, group = .$visitno, id = .$pubID,
#'                method = 'mcnemar', num_needed_for_test = 3, digits = 1,
#'                trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#'
#' # ICS Assay Data Example
#' data(exampleData_ICS)
#'
#' ## Group Comparison
#'group_testing <- exampleData_ICS %>%
#'    group_by(Stim, Parent, Population, Visit) %>%
#'    group_modify(~ as.data.frame(
#'        pairwise_test_bin(x = .$response, group = .$Group , alternative = 'greater',
#'                method = 'barnard', num_needed_for_test = 3, digits = 1,
#'                trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#'
#' ## Timepoint Comparison
#'timepoint_testing <- exampleData_ICS %>%
#'    group_by(Stim, Parent, Population, Group) %>%
#'    group_modify(~ as.data.frame(
#'        pairwise_test_bin(x = .$response, group = .$Visit, id = .$pubID,
#'                method = 'mcnemar', num_needed_for_test = 3, digits = 1,
#'                trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#'
#' @export


pairwise_test_bin <- function(x, group, id = NULL,
                                 method = c('barnard', 'fisher' ,'chi.sq' , 'mcnemar'),
                                 barnard_method = c("z-pooled", "z-unpooled", "boschloo",
                                     "santner and snell", "csm", "csm approximate", "csm modified"),
                                 alternative = c("two.sided", "less", "greater"),
                                 sorted_group = NULL, num_needed_for_test = 3, conf_level = 0.95,
                                 digits = 1, trailing_zeros = TRUE, sep_val = " vs. ",
                                 na_str_out = "---", latex_output = FALSE, verbose = FALSE, ...){
  #Input Checking
  method <- match.arg(method)
  barnard_method <- match.arg(barnard_method)
  alternative <- match.arg(alternative)
  if (method == 'chi.sq' & alternative != 'two.sided')
    stop('When "method" is chi.sq then "alternative" must be two.sided')
  if (method == 'mcnemar' & alternative != 'two.sided')
    stop('When "method" is mcnemar then "alternative" must be two.sided')

  # If all NA return NULL
  if (all(is.na(x))) return(NULL)
  .check_response_input(x)

  if (!is.null(sorted_group)) {
    if (any(!group %in% c(NA,sorted_group)))
      stop('"sorted_group" must contain all possible values of "group"')
  }
  if (is.null(sorted_group) & (alternative != 'two.sided') & verbose)
    message('"sorted_group" not specified so testing in following order: ',
            paste0(levels(group), collapse = ', '))

  if (method == 'mcnemar' & is.null(id))
    stop('"id" must be present when "method" is mcnemar')

  if (method == 'mcnemar' & length(unique(id)) == length(x))
    stop('"id" must have duplicated values when "method" is mcnemar')

  # Setting Latex Ouput suffix
  if (latex_output) suffix <- '\\%' else suffix <- '%'

  #Dropping any missing in either x or group
  if (method == 'mcnemar')
    keep_index <- !is.na(x) & !is.na(group) & !is.na(id) else
      keep_index <- !is.na(x) & !is.na(group)
  x <- x[keep_index]
  group <- group[keep_index]
  if (method == 'mcnemar') id <- id[keep_index]

  #Need to set factor levels so can provide directional one-sided tests if needed
  if (is.null(sorted_group))
    group <- droplevels(factor(group)) else
      group <- droplevels(
        factor(group, levels = sorted_group[!is.na(match(sorted_group, group))])
      )
  n_levels <- nlevels(group)
  levels_here <- levels(group)

  if (n_levels == 0) {
    if (verbose)
      message('No non-missing values, so nothing to compare')
    return(NULL)
  }
  if (n_levels == 1) {
    if (verbose)
      message('Only one group has any non-missing values, so nothing to compare')
    return(NULL)
  }

  results_list <- list()
  for (i in 1:(n_levels - 1)) {
    for (j in (i + 1):n_levels) {
      i_group <- levels_here[i]
      j_group <- levels_here[j]

      if (method == 'mcnemar') {
        #For paired testing only using non-missing values in both groups
        i_data <- data.frame(x = x[group == i_group],
                             id = id[group == i_group])
        j_data <- data.frame(y = x[group == j_group],
                             id = id[group == j_group])
        data_here <- stats::na.omit(merge(i_data, j_data, by = 'id'))
        i_vals <- data_here$x
        j_vals <- data_here$y
        vals_here <-  c(i_vals, j_vals)
        groups_here <-  c(rep(i_group, nrow(data_here)),
                          rep(j_group, nrow(data_here)))
      } else {
        i_vals <- x[group == i_group]
        j_vals <- x[group == j_group]
        vals_here <-  x[group  %in% c(i_group, j_group)]
        groups_here <-
          factor(group[group  %in% c(i_group, j_group)],
                 levels = c(i_group, j_group))
      }

      #Getting perfect seperation flag
      if (length(unique(groups_here)) == 2) {
        if ((all(i_vals == 0) & all(j_vals == 1)) |
            (all(i_vals == 1) & all(j_vals == 0)))
          perfect_seperation <- TRUE else
            perfect_seperation <- FALSE
      } else {
        perfect_seperation <- NA
      }

      # Getting response rate with wilson CI
      response_info_here_by_group <- by(vals_here, groups_here, function(xx)
      {
        wilson_est <- wilson_ci(xx, conf_level)
        paste0(sum(xx), '/', length(xx), ' = ',
          stat_paste(wilson_est$mean * 100, wilson_est$lower * 100, wilson_est$upper * 100,
                   digits = digits, trailing_zeros = trailing_zeros, suffix = suffix)
        )
      })

      stats_by_group <- data.frame(Group1 = i_group,
                                   Group2 = j_group,
                                   Group1_rr = response_info_here_by_group[[1]],
                                   Group2_rr = response_info_here_by_group[[2]],
                                   stringsAsFactors = FALSE
      )

      #All conditions needed for test to run
      if ((length(i_vals) >= num_needed_for_test) &
          (length(j_vals) >= num_needed_for_test)) {

        #Need to flip one sided alternative for fisher's testing
        testing_alternative <- switch(alternative,
                                      two.sided = 'two.sided',
                                      less = 'greater',
                                      greater = 'less')

        response_p <- two_samp_bin_test(x = vals_here, y = groups_here,
                                        method = method,
                                        alternative = testing_alternative,
                                        verbose = verbose)
      } else {
        if (verbose)
          message(paste0('x does not have at least ',
                         num_needed_for_test,
                         ' non missing values per group, so no test run (ResponseTest=NA returned)'))
        response_p <- NA_real_
      }


      results_list[[length(results_list) + 1]] <-
        data.frame(stats_by_group, `ResponseTest` = response_p,
                   PerfectSeperation = perfect_seperation,
                   stringsAsFactors = FALSE)

    }
  }

  results <- do.call(base::rbind, results_list)

  # Pasting together stats
  pasted_results <- paste_tbl_grp(data = results,
                                  sep_val = sep_val,
                                  alternative = alternative,
                                  digits = digits,
                                  trailing_zeros = trailing_zeros,
                                  keep_all = TRUE, verbose = FALSE)


  data.frame(Comparison = pasted_results$Comparison,
             ResponseStats = pasted_results$rr_comparison,
             ResponseTest = results$ResponseTest ,
             PerfectSeperation = results$PerfectSeperation,
             stringsAsFactors = FALSE)

}


