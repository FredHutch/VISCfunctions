context("pairwise_comparisons")


# test paste_tbl_grp. Using paste_tbl_grp and pairwise_test_cont in testing since these functions are testing elsewhere
test_that("pairwise_comparisons testing two groups", {
  set.seed(243542534)
  x = c(NA, rnorm(25, 0, 1), rnorm(25, 1, 1),NA)
  group = c(rep('a', 26),rep('b', 26))
  id = c(1:26, 1:26)
  test_data <- data.table(x, group)

  testing_stats <- test_data[!is.na(x), .(
    Group1 = unique(group[group == 'a']), Group2 = unique(group[group == 'b']),
    Group1_n = length(x[group == 'a']), Group2_n = length(x[group == 'b']),
    Group1_mean = mean(x[group == 'a']), Group2_mean = mean(x[group == 'b']),
    Group1_sd = sd(x[group == 'a']), Group2_sd = sd(x[group == 'b']),
    Group1_median = median(x[group == 'a']), Group2_median = median(x[group == 'b']),
    Group1_min = min(x[group == 'a']), Group2_min = min(x[group == 'b']),
    Group1_max = max(x[group == 'a']), Group2_max = max(x[group == 'b']),
    Group1_IQR = IQR(x[group == 'a'])
  )]

  # Defaults
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 0, keep_all = FALSE, trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = FALSE),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
)
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'two.sided', num_needed_for_test = 3, digits = 0, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # Digits to 3
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = FALSE),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'two.sided', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # less than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE, alternative = 'less')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = FALSE, alternative = 'less'),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'less', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # greater than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE, alternative = 'greater')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = FALSE, alternative = 'greater'),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'greater', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)


  # sorted group less than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE, first_name = 'Group2', second_name = 'Group1', alternative = 'less')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = factor(group, levels = c('b','a')), method = 'wilcox', paired = FALSE, alternative = 'less'),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'less', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE, sorted_group = c('b','a')),
               expected = testing_results)


  # t.test
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 't.test', paired = FALSE),
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 't.test', alternative = 'two.sided', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)


  # High number needed for testing
  test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = NA_integer_,
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = FALSE, method = 'wilcox', alternative = 'two.sided', num_needed_for_test = 100, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)



  # Paired data
  paired_ids <- na.omit(data.frame(x, group, id))$id[(duplicated(na.omit(data.frame(x, group, id))$id))]
  testing_stats_paired <- test_data[id %in% paired_ids, .(
    Group1 = unique(group[group == 'a']), Group2 = unique(group[group == 'b']),
    Group1_n = length(x[group == 'a']), Group2_n = length(x[group == 'b']),
    Group1_mean = mean(x[group == 'a']), Group2_mean = mean(x[group == 'b']),
    Group1_sd = sd(x[group == 'a']), Group2_sd = sd(x[group == 'b']),
    Group1_median = median(x[group == 'a']), Group2_median = median(x[group == 'b']),
    Group1_min = min(x[group == 'a']), Group2_min = min(x[group == 'b']),
    Group1_max = max(x[group == 'a']), Group2_max = max(x[group == 'b']),
    Group1_IQR = IQR(x[group == 'a'])
  )]

  test_pasting <- paste_tbl_grp(data = testing_stats_paired, vars_to_paste = c('median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 3, keep_all = FALSE, trailing_zeros = TRUE)
  testing_results <- data.frame(Comparison = test_pasting$Comparison,
                                SampleSizes =  sum(duplicated(na.omit(data.frame(x, group, id))$id)),
                                Median_Min_Max = test_pasting$median_min_max_comparison,
                                Mean_SD = test_pasting$mean_sd_comparison,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = TRUE),
                                PerfectSeperation = ifelse((testing_stats_paired$Group1_min >  testing_stats_paired$Group2_max) | (testing_stats_paired$Group2_min >  testing_stats_paired$Group1_max), TRUE, FALSE),
                                stringsAsFactors = FALSE
  )
  expect_equal(object = pairwise_test_cont(x = x, group = group, paired = TRUE, id = id, method = 'wilcox', alternative = 'two.sided', num_needed_for_test = 3, digits = 3, trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

})


test_that("Integration with dplyr and data.table is equivalent", {
  # Comparing data.tabel and dplyr (BAMA Assay Data Example)
  library(dplyr)
  data(exampleData_BAMA)

  ## Group Comparison
  # using data.table
  group_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
    x = magnitude, group = group, paired = FALSE, method = 'wilcox',
    alternative = 'less', num_needed_for_test = 3, digits = 3,
    trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
  ),
  by = .(antigen, visitno)][order(antigen, visitno)]

  # using dplyr
  group_testing_tibble <- exampleData_BAMA %>%
    group_by(antigen, visitno) %>%
    do(pairwise_test_cont(x = .$magnitude, group = .$group, paired = F, method = 'wilcox', alternative = "less", digits = 3, num_needed_for_test = 3, verbose = TRUE))
  # Confirming both methods are the same
  expect_equal(object = group_testing_dt[order(antigen, visitno)],
               expected = data.table(group_testing_tibble)[order(antigen, visitno)])

})







# test paste_tbl_grp. Using paste_tbl_grp and pairwise_test_cont in testing since these functions are testing elsewhere
test_that("pairwise_comparisons testing multiple groups", {

   data("testData_BAMA")

  test_single_comp <- function(x, group, Group1, Group2) {
    test_data <- data.table(x = x, group = group)

    testing_stats <- test_data[!is.na(x), .(
      Group1 = unique(group[group == Group1]), Group2 = unique(group[group == Group2]),
      Group1_n = length(x[group == Group1]), Group2_n = length(x[group == Group2]),
      Group1_mean = mean(x[group == Group1]), Group2_mean = mean(x[group == Group2]),
      Group1_sd = sd(x[group == Group1]), Group2_sd = sd(x[group == Group2]),
      Group1_median = median(x[group == Group1]), Group2_median = median(x[group == Group2]),
      Group1_min = min(x[group == Group1]), Group2_min = min(x[group == Group2]),
      Group1_max = max(x[group == Group1]), Group2_max = max(x[group == Group2]),
      Group1_IQR = IQR(x[group == Group1])
    )]

    # Defaults
    test_pasting <- paste_tbl_grp(data = testing_stats, vars_to_paste = c('n','median_min_max', 'mean_sd'), sep_val = " vs. ", digits = 0, keep_all = FALSE, trailing_zeros = TRUE)
    names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
    testing_results <- data.frame(test_pasting,
                                  MagnitudeTest = two_samp_cont_test(x = x[group %in% c(Group1,Group2)], y = group[group %in% c(Group1,Group2)], method = 'wilcox', paired = FALSE),
                                  PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) | (testing_stats$Group2_min >  testing_stats$Group1_max), TRUE, FALSE)
    )
    testing_results
  }

  test_all_comp <- function(x, group){
    # print(x);print(group);print(group[!is.na(x)])
    x_here <- x[!is.na(x)]
    group_here <- droplevels(group[!is.na(x)])
    if (length(unique(group[!is.na(x)])) > 1) {
      test_results_paste <- list()
      for (i in 1:(nlevels(group_here) - 1)) {
        for (j in ((i + 1):nlevels(group_here))) {
          # print(test_single_comp(x = x_here, group = group_here, Group1 = levels(group_here)[i], Group2 = levels(group_here)[j]))
          test_results_paste[[length(test_results_paste) + 1]] <- test_single_comp(x = x_here, group = group_here, Group1 = levels(group_here)[i], Group2 = levels(group_here)[j])
        }
      }
      do.call(base::rbind, test_results_paste)
    }
  }

  testing_results <- testData_BAMA[, test_all_comp(
    x = magnitude, group = group
  ),
  by = .(antigen, visit)][order(antigen, visit)]

  group_testing_dt <- testData_BAMA[, pairwise_test_cont(
    x = magnitude, group = group, paired = FALSE, method = 'wilcox',
    alternative = 'two.sided', num_needed_for_test = 3, digits = 0,
    trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE
  ),
  by = .(antigen, visit)][order(antigen, visit)]

  expect_equal(object = group_testing_dt,
               expected = testing_results)


})

test_that("Test example with fixed result", {

  source("fixed_data.R")


  library(VISCfunctions.data)
  data("exampleData_BAMA")

  group_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
     x = magnitude, group = group, paired = FALSE, method = 'wilcox',
     alternative = 'less', num_needed_for_test = 3, digits = 3,
     trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
    ),
    by = .(antigen, visitno)][order(antigen, visitno)]

  expect_equal(object = group_testing_dt,
               expected = fixed_bama_group_testing_dt)



  })

test_that("Integration with dplyr and data.table is equivalent", {

  library(VISCfunctions.data)
  library(data.table)
  library(dplyr)
  
  data(exampleData_BAMA)
  
  group_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
   x = magnitude, group = group, paired = FALSE, method = 'wilcox',
   alternative = 'less', num_needed_for_test = 3, digits = 3,
   trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
  ),
  by = .(antigen, visitno)][order(antigen, visitno)]
  
  # using dplyr
  group_testing_tibble <- exampleData_BAMA %>%
     group_by(antigen, visitno) %>%
     do(pairwise_test_cont(x = .$magnitude, group = .$group, paired = F, method = 'wilcox', alternative = "less", digits = 3, num_needed_for_test = 3, verbose = TRUE))

# Confirming both methods are the same
expect_equal(object = group_testing_dt[order(antigen, visitno)],
             expected = data.table(group_testing_tibble)[order(antigen, visitno)])

  
})

test_that("Paired results with test data"){
  library(dplyr)
  
  data("testData_BAMA")
  paired_example = subset(testData_BAMA, visit %in% c(1, 2) & antigen == "1086C_D7gp120.avi/293F")
  
  paired_example_subset1 = subset(paired_example,  visit == 1, select = -response)
  paired_example_subset2 = subset(paired_example,  visit == 2, select = -response)
  paired_data = merge(paired_example_subset1, paired_example_subset2, by = c("pubID", "antigen", "group")) 
  
  paired_tests_ls = list()
  for(i in 1:length(unique(paired_data$group))){
    temp_dat = subset(paired_data, group == unique(paired_data$group)[i])
    
    paired_tests_ls[[i]] = 
      data.frame(
        group = unique(paired_data$group)[i],
        total = nrow(na.omit(temp_dat)),
        test = pvalue(wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y, distribution = "exact", zero.method = "Pratt")),
        est_less =  pvalue(wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y, distribution = "exact", zero.method = "Pratt", alternative = "less"))
      )

  }
  paired_tests = do.call(rbind, paired_tests_ls)
  
  group_testing_tibble <- paired_example %>%
     group_by(group) %>%
     do(pairwise_test_cont(x = .$magnitude, group = .$visit, paired = T, id = .$pubID, digits = 3, num_needed_for_test = 2, verbose = TRUE)) %>%
    left_join(paired_tests, by = "group")

  expect_equal(group_testing_tibble$SampleSizes, group_testing_tibble$total)
  expect_equal(group_testing_tibble$MagnitudeTest, group_testing_tibble$test)
  
  group_testing_tibble_less <- paired_example %>%
     group_by(group) %>%
     do(pairwise_test_cont(x = .$magnitude, group = .$visit, paired = T, id = .$pubID, digits = 3, alternative = "less",
                           num_needed_for_test = 2, verbose = TRUE)) %>%
    left_join(paired_tests, by = "group")
  expect_equal(group_testing_tibble_less$SampleSizes, group_testing_tibble_less$total)
  expect_equal(group_testing_tibble_less$MagnitudeTest, group_testing_tibble_less$est_less)  
  
}
