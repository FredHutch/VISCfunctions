context("pairwise_comparisons")


# test pairwise_test_cont. Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons_bin testing two groups", {
  library(tidyr)
  library(dplyr)
  set.seed(243542534)
  x = c(NA, rnorm(25, 0, 1), rnorm(25, 1, 1),NA)
  x_high = c(NA, rnorm(25, 100, 1), rnorm(25, 150, 1),NA)
  group = c(rep('a', 26),rep('b', 26))
  id = c(1:26, 1:26)

  test_data <- data.frame(x, group)

  testing_stats <- test_data %>%
    filter(!is.na(x)) %>%
    group_by(group) %>%
    summarise(n = n(),
              mean = mean(x, na.rm = TRUE),
              sd = sd(x, na.rm = TRUE),
              median = median(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE),
              IQR = IQR(x, na.rm = TRUE),
              .groups = "keep") %>%
    pivot_wider(names_from = group,
                values_from = c(n, mean, sd, median, min, max, IQR)) %>%
    mutate(Group1 = "a", Group2 = "b", .before = "n_a")

  colnames(testing_stats)[3:16] <- c("Group1_n", "Group2_n", "Group1_mean",
                                "Group2_mean", "Group1_sd", "Group2_sd",
                                "Group1_median", "Group2_median",
                                "Group1_min", "Group2_min", "Group1_max",
                                "Group2_max", "Group1_IQR", "Group2_IQR")

  # Defaults
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ",
                                digits = 0,
                                keep_all = FALSE,
                                trailing_zeros = TRUE)

  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = group,
                                                                   method = 'wilcox',
                                                                   paired = FALSE),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))

  expect_equal(object = test_data %>% pairwise_test_cont(x = x,
                                           group = group, paired = FALSE,
                                           method = 'wilcox',
                                           alternative = 'two.sided',
                                           num_needed_for_test = 3,
                                           digits = 0, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # Digits to 3
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = FALSE,
                                trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = group,
                                                                   method = 'wilcox',
                                                                   paired = FALSE),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))

  expect_equal(object = test_data %>% pairwise_test_cont(x = x,
                                           group = group,
                                           paired = FALSE,
                                           method = 'wilcox',
                                           alternative = 'two.sided',
                                           num_needed_for_test = 3,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # Less than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = FALSE,
                                trailing_zeros = TRUE,
                                alternative = 'less')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = group,
                                                                   method = 'wilcox',
                                                                   paired = FALSE,
                                                                   alternative = 'less'),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))
  expect_equal(object = test_data %>% pairwise_test_cont(x = x,
                                           group = group,
                                           paired = FALSE,
                                           method = 'wilcox',
                                           alternative = 'less',
                                           num_needed_for_test = 3,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # Greater than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ", digits = 3,
                                keep_all = FALSE, trailing_zeros = TRUE,
                                alternative = 'greater')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = group,
                                                                   method = 'wilcox',
                                                                   paired = FALSE,
                                                                   alternative = 'greater'),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))
  expect_equal(object = test_data %>% pairwise_test_cont(x = x,
                                           group = group,
                                           paired = FALSE,
                                           method = 'wilcox',
                                           alternative = 'greater',
                                           num_needed_for_test = 3,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)


  # Sorted group, less than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ", digits = 3,
                                keep_all = FALSE, trailing_zeros = TRUE,
                                first_name = 'Group2', second_name = 'Group1',
                                alternative = 'less')
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = factor(group, levels = c('b','a')),
                                                                   method = 'wilcox',
                                                                   paired = FALSE,
                                                                   alternative = 'less'),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))
  expect_equal(object = test_data %>% pairwise_test_cont(x = x, group = group,
                                           paired = FALSE, method = 'wilcox',
                                           alternative = 'less',
                                           num_needed_for_test = 3, digits = 3,
                                           trailing_zeros = TRUE, sep_val = ' vs. ',
                                           verbose = FALSE,  sorted_group = c('b','a')),
               expected = testing_results)

  # t.test
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = FALSE,
                                trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = two_samp_cont_test(x = x,
                                                                   y = group,
                                                                   method = 't.test',
                                                                   paired = FALSE),
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))

  expect_equal(object = test_data %>% pairwise_test_cont(x = x, group = group, paired = FALSE,
                                           method = 't.test', alternative = 'two.sided',
                                           num_needed_for_test = 3, digits = 3,
                                           trailing_zeros = TRUE, sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)


  # High number needed for testing
  test_pasting <- paste_tbl_grp(data = testing_stats,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ", digits = 3,
                                keep_all = FALSE, trailing_zeros = TRUE)
  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  testing_results <- data.frame(test_pasting,
                                MagnitudeTest = NA_integer_,
                                PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))
  expect_equal(object = test_data %>% pairwise_test_cont(x = x, group = group,
                                           paired = FALSE, method = 'wilcox',
                                           alternative = 'two.sided', num_needed_for_test = 100,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
               expected = testing_results)

  # Testing log10 stats
  set.seed(53276537)
  x_high = c(NA, rnorm(25, 100, 1), rnorm(25, 150, 1),NA)

  test_data_log <- data.frame(x_high, group)

  testing_stats_log <- test_data_log %>%
    filter(!is.na(x)) %>%
    mutate(x = log10(x_high)) %>%
    group_by(group) %>%
    summarise(n = n(),
              mean = mean(x, na.rm = TRUE),
              median = median(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE),
              IQR = IQR(x, na.rm = TRUE),
              logmean = mean(x, na.rm = TRUE),
              logsd = sd(x, na.rm = TRUE),
              .groups = "keep") %>%
    pivot_wider(names_from = group,
                values_from = c(n, mean, median, min, max, IQR, logmean, logsd)) %>%
    mutate(across(mean_a:IQR_b, .fns = ~10^.x),
           Group1 = "a", Group2 = "b",
           Group1log = "a", Group2log = "b", .before = "n_a")

  colnames(testing_stats_log)[5:20] <- c("Group1_n", "Group2_n", "Group1_mean",
                                     "Group2_mean",
                                     "Group1_median", "Group2_median",
                                     "Group1_min", "Group2_min", "Group1_max",
                                     "Group2_max", "Group1_IQR", "Group2_IQR",
                                     "Group1log_mean", "Group2log_mean",
                                     "Group1log_sd", "Group2log_sd")

  test_pasting <- paste_tbl_grp(data = testing_stats_log,
                                vars_to_paste = c('n','median_min_max', 'mean'),
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = FALSE,
                                trailing_zeros = TRUE)

  test_pasting_extra <- paste_tbl_grp(data = testing_stats_log,
                                vars_to_paste = c('mean_sd'),
                                first_name = 'Group1log',
                                second_name = 'Group2log',
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = FALSE,
                                trailing_zeros = TRUE)

  names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean')
  testing_results <- data.frame(test_pasting,
                                log_Mean_SD = test_pasting_extra$mean_sd_comparison,
                                MagnitudeTest = two_samp_cont_test(x = log10(x_high),
                                                                   y = group,
                                                                   method = 'wilcox',
                                                                   paired = FALSE),
                                PerfectSeparation = ifelse((testing_stats_log$Group1_min >  testing_stats_log$Group2_max) |
                                                             (testing_stats_log$Group2_min >  testing_stats_log$Group1_max),
                                                           TRUE, FALSE),
                                stringsAsFactors = FALSE)

  expect_equal(object = pairwise_test_cont(x = test_data_log$x_high,
                                           group = test_data_log$group, paired = FALSE,
                                           method = 'wilcox',
                                           alternative = 'two.sided',
                                           num_needed_for_test = 3,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE,
                                           log10_stats = TRUE),
               expected = testing_results)


  # Paired data

  test_ids <- test_data %>% mutate(id = rep(1:26, 2)) %>% filter(!is.na(x))
  dup_id <- test_ids[duplicated(test_ids$id),]
  testing_stats_paired <- test_ids %>% filter(id %in% dup_id$id) %>%
    group_by(group) %>%
    summarise(n = n(),
              mean = mean(x, na.rm = TRUE),
              sd = sd(x, na.rm = TRUE),
              median = median(x, na.rm = TRUE),
              min = min(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE),
              IQR = IQR(x, na.rm = TRUE),
              .groups = "keep") %>%
    pivot_wider(names_from = group,
                values_from = c(n, mean, sd, median, min, max, IQR)) %>%
    mutate(Group1 = "a", Group2 = "b", .before = "n_a")

  colnames(testing_stats_paired)[3:16] <- c("Group1_n", "Group2_n", "Group1_mean",
                                     "Group2_mean", "Group1_sd", "Group2_sd",
                                     "Group1_median", "Group2_median",
                                     "Group1_min", "Group2_min", "Group1_max",
                                     "Group2_max", "Group1_IQR", "Group2_IQR")


  test_pasting <- paste_tbl_grp(data = testing_stats_paired,
                                vars_to_paste = c('median_min_max', 'mean_sd'),
                                sep_val = " vs. ", digits = 3, keep_all = FALSE,
                                trailing_zeros = TRUE)

  # test group order
  test_pasting_rev <- paste_tbl_grp(data = testing_stats_paired,
                                vars_to_paste = c('median_min_max', 'mean_sd'),
                                sep_val = " vs. ", digits = 3, keep_all = FALSE,
                                first_name = "Group2", second_name = "Group1",
                                trailing_zeros = TRUE)

  expect_false(identical(test_pasting, test_pasting_rev))

  testing_results <- data.frame(Comparison = test_pasting$Comparison,
                                SampleSizes =  sum(duplicated(na.omit(data.frame(x, group, id))$id)),
                                Median_Min_Max = test_pasting$median_min_max_comparison,
                                Mean_SD = test_pasting$mean_sd_comparison,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = TRUE),
                                PerfectSeparation = ifelse((testing_stats_paired$Group1_min >
                                                              testing_stats_paired$Group2_max) |
                                                             (testing_stats_paired$Group2_min >
                                                                testing_stats_paired$Group1_max), TRUE, FALSE),
                                stringsAsFactors = FALSE)

  expect_equal(object = pairwise_test_cont(x = x, group = group,
                                           paired = TRUE, id = id,
                                           sorted_group = c("a", "b"),
                                           method = 'wilcox', alternative = 'two.sided',
                                           num_needed_for_test = 3, digits = 3,
                                           trailing_zeros = TRUE, sep_val = ' vs. ',
                                           verbose = FALSE),
               expected = testing_results)

  testing_results_rev <- data.frame(Comparison = test_pasting_rev$Comparison,
                                    SampleSizes =  sum(duplicated(na.omit(data.frame(x, group, id))$id)),
                                    Median_Min_Max = test_pasting_rev$median_min_max_comparison,
                                    Mean_SD = test_pasting_rev$mean_sd_comparison,
                                    MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = TRUE),
                                    PerfectSeparation = ifelse((testing_stats_paired$Group1_min >
                                                                  testing_stats_paired$Group2_max) |
                                                                 (testing_stats_paired$Group2_min >
                                                                    testing_stats_paired$Group1_max), TRUE, FALSE),
                                    stringsAsFactors = FALSE)

  expect_equal(object = pairwise_test_cont(x = x, group = factor(group, levels = c("b", "a")),
                                           paired = TRUE, id = id,
                                           #sorted_group = c("a", "b"),
                                           method = 'wilcox', alternative = 'two.sided',
                                           num_needed_for_test = 3, digits = 3,
                                           trailing_zeros = TRUE, sep_val = ' vs. ',
                                           verbose = FALSE),
               expected = testing_results_rev)
})

# test pairwise_test_cont. Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons testing multiple groups", {
  library(tidyr)

  test_single_comp <- function(x, group, Group1, Group2) {
    test_data <- data.frame(x = x[!is.na(x)],
                            group = group[!is.na(x)])

    testing_stats <- test_data %>%
      summarise(
        Group1 = unique(group[group == Group1]), Group2 = unique(group[group == Group2]),
        Group1_n = length(x[group == Group1]), Group2_n = length(x[group == Group2]),
        Group1_mean = mean(x[group == Group1]), Group2_mean = mean(x[group == Group2]),
        Group1_sd = sd(x[group == Group1]), Group2_sd = sd(x[group == Group2]),
        Group1_median = median(x[group == Group1]), Group2_median = median(x[group == Group2]),
        Group1_min = min(x[group == Group1]), Group2_min = min(x[group == Group2]),
        Group1_max = max(x[group == Group1]), Group2_max = max(x[group == Group2]),
        Group1_IQR = IQR(x[group == Group1])
      )

    # Defaults
    test_pasting <- paste_tbl_grp(data = testing_stats,
                                  vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                  sep_val = " vs. ", digits = 3, keep_all = FALSE,
                                  trailing_zeros = TRUE)
    names(test_pasting) <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
    testing_results <- data.frame(
      test_pasting,
      MagnitudeTest = two_samp_cont_test(x = x[group %in% c(Group1,Group2)],
                                         y = group[group %in% c(Group1,Group2)],
                                         method = 'wilcox', paired = FALSE),
      PerfectSeparation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                   (testing_stats$Group2_min >  testing_stats$Group1_max),
                                 TRUE,
                                 FALSE)
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

  testing_results <- testData_BAMA %>%
    group_by(antigen, visit) %>%
    group_modify(~test_all_comp(x = .x$magnitude, group = .x$group) %>%
                   as.data.frame)

  group_testing_dt <- testData_BAMA %>%
    group_by(antigen, visit) %>%
    group_modify(~pairwise_test_cont(x = .x$magnitude,
                                     group = .x$group,
                                     paired = FALSE,
                                     method = 'wilcox',
                                     alternative = 'two.sided',
                                     num_needed_for_test = 3,
                                     digits = 3,
                                     trailing_zeros = TRUE,
                                     sep_val = ' vs. ',
                                     verbose = FALSE) %>%
                   as.data.frame)

  expect_equal(object = group_testing_dt,
               expected = testing_results)

})

test_that("Test example with fixed result", {

  data(exampleData_BAMA)
  source("fixed_data.R")
  fixed_bama_group_testing_dt <- as_tibble(fixed_bama_group_testing_dt) %>%
    arrange(antigen)

  group_testing_dt <- exampleData_BAMA %>%
    group_by(antigen, visitno) %>%
    summarise(pairwise_test_cont(x = magnitude,
                                 group = group,
                                 paired = FALSE,
                                 method = 'wilcox',
                                 alternative = 'less',
                                 sorted_group = c(1, 2),
                                 num_needed_for_test = 3,
                                 digits = 3,
                                 trailing_zeros = TRUE,
                                 sep_val = ' vs. ',
                                 verbose = TRUE),
              .groups = "drop_last") %>%
    ungroup()

  expect_equal(object = group_testing_dt,
               expected = fixed_bama_group_testing_dt)
  })


test_that("Paired results with test data", {
  library(dplyr)

  paired_example <- exampleData_BAMA %>%
    filter(visitno != 0, antigen == "B.63521_D11gp120/293F")
  paired_example_subset1 <- subset(paired_example,  visitno == 1, select = -response)
  paired_example_subset2 <- subset(paired_example,  visitno == 2, select = -response)
  paired_data <- merge(paired_example_subset1,
                      paired_example_subset2,
                      by = c("pubID", "antigen", "group"))

  paired_tests_ls = list()
  for (i in 1:length(unique(paired_data$group))) {
    temp_dat = subset(paired_data,
                      group == unique(paired_data$group)[i])

    paired_tests_ls[[i]] =
      data.frame(
        group = unique(paired_data$group)[i],
        total = nrow(na.omit(temp_dat)),
        test = as.numeric(coin::pvalue(coin::wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y,
                                                             distribution = "exact",
                                                             zero.method = "Pratt"))),
        est_less =  as.numeric(coin::pvalue(coin::wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y,
                                                                  distribution = "exact",
                                                                  zero.method = "Pratt",
                                                                  alternative = "less"))))
  }
  paired_tests <- do.call(rbind, paired_tests_ls)

  group_testing_tibble <- paired_example %>%
     group_by(group) %>%
     summarise(pairwise_test_cont(x = magnitude, group = visitno,
                           paired = TRUE,
                           id = pubID,
                           digits = 3,
                           num_needed_for_test = 2,
                           verbose = TRUE),
               .groups = "keep") %>%
    left_join(paired_tests, by = "group")

  expect_equal(group_testing_tibble$SampleSizes,
               group_testing_tibble$total)
  expect_equal(group_testing_tibble$MagnitudeTest,
               group_testing_tibble$test)

  group_testing_tibble_less <- paired_example %>%
     group_by(group) %>%
     summarise(pairwise_test_cont(x = magnitude, group = visitno,
                           paired = TRUE,
                           id = pubID,
                           digits = 3,
                           alternative = "less",
                           sorted_group = c(1, 2),
                           num_needed_for_test = 2,
                           verbose = TRUE),
               .groups = "keep") %>%
    left_join(paired_tests, by = "group")
  expect_equal(group_testing_tibble_less$SampleSizes,
               group_testing_tibble_less$total)
  expect_equal(group_testing_tibble_less$MagnitudeTest,
               group_testing_tibble_less$est_less)
})


test_that("pairwise_comparisons_bin error catching and messages", {

  expect_error(object = pairwise_test_cont(x = 1:10, group = 0:1),
               regexp = '"x" and "group" must be same length')

  expect_error(object = pairwise_test_cont(x = 1:10, group = rep(0:1, 5),
                                           paired = TRUE),
               regexp = '"id" must be present when "paired" = TRUE')

  expect_error(object = pairwise_test_cont(x = 1:10, group = rep(0:1, 5),
                                           sorted_group = 1),
               regexp = '"sorted_group" must contain all possible values of "group"')

  expect_error(object = pairwise_test_cont(x = -1:-10, group = rep(0:1, 5),
                                           log10_stats = TRUE),
               regexp = '"x" must be greater than 0 when "log10_stats" = TRUE')

  expect_message(object = pairwise_test_cont(x = 1:10, group = rep(0:1, 5),
                                             alternative = 'less', verbose = TRUE),
               regexp = '"sorted_group" not specified so testing in following order: 0, 1')

  expect_message(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,NA,NA, 1),
                                             verbose = TRUE),
                 regexp = 'No non-missing values, so nothing to compare')
  expect_null(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,NA,NA, 1)))

  expect_message(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,NA,1,NA),
                                             verbose = TRUE),
                 regexp = 'Only one group has any non-missing values, so nothing to compare')
  expect_null(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,NA,1,NA)))

  expect_message(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,0,1,1),
                                             verbose = TRUE),
                 regexp = 'x does not have at least 3 non missing per group, so no test run \\(MagnitudeTest=NA returned\\)')

  expect_message(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,0,1,1),
                                             paired = TRUE, id = c('a','b','a','b'),
                                             verbose = TRUE),
                 regexp = 'No paired samples for levels 0 and 1')

  expect_null(object = pairwise_test_cont(x = c(NA,1,1,NA), group = c(0,0,1,1),
                                             paired = TRUE, id = c('a','b','a','b')))

})

# test pairwise_test_bin.
#Using paste_tbl_grp and two_samp_bin_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons_bin testing two groups", {
  library(dplyr)
  set.seed(243542534)
  x = c(NA,
        sample(0:1,25,replace = TRUE, prob = c(.75,.25)),
        NA,
        sample(0:1,25,replace = TRUE, prob = c(.25,.75)))
  group = c(rep('a', 26),rep('b', 26))
  id = c(1:26, 1:26)
  test_data <- tibble(x, group, id)

  testing_stats_pre <- test_data %>%
    filter(!is.na(x)) %>%
    group_by(group) %>%
    mutate(num_pos = sum(x), n = n()) %>%
    group_by(group,num_pos, n) %>%
    group_modify( ~ wilson_ci(.$x, .95)) %>% ungroup() %>%
    mutate(rr = paste0(num_pos, '/', n, ' = ',
                       stat_paste(mean * 100, lower * 100, upper * 100, digits = 1, suffix = '%')))

  testing_stats <- bind_cols(
    testing_stats_pre %>% filter(group == 'a') %>% select(Group1 = group, Group1_rr = rr),
    testing_stats_pre %>% filter(group == 'b') %>% select(Group2 = group, Group2_rr = rr))


  # testing multiple methods
  test_pasting <- paste_tbl_grp(data = testing_stats)

  names(test_pasting) <- c('Comparison', 'ResponseStats')

  lapply(c("barnard", "fisher", "chi.sq"),
              function(method_in){
                testing_results <- data.frame(test_pasting,
                                              ResponseTest  = two_samp_bin_test(x = x,
                                                                                y = group,
                                                                                method = method_in),
                                              PerfectSeparation = ifelse(diff(testing_stats_pre$mean) == 1,
                                                                         TRUE, FALSE))
                expect_equal(object = pairwise_test_bin(x = x, group = group, method = method_in),
                             expected = testing_results)
                })

  # Digits to 3
  testing_stats_pre_3digits <- test_data %>%
    filter(!is.na(x)) %>%
    group_by(group) %>%
    mutate(num_pos = sum(x), n = n()) %>%
    group_by(group,num_pos, n) %>%
    group_modify( ~ wilson_ci(.$x, .95)) %>% ungroup() %>%
    mutate(rr = paste0(num_pos, '/', n, ' = ',
                       stat_paste(mean * 100, lower * 100, upper * 100, digits = 3, suffix = '%')))

  testing_stats_3digits <- bind_cols(
    testing_stats_pre_3digits %>%
    filter(group == 'a') %>%
    select(Group1 = group, Group1_rr = rr), testing_stats_pre_3digits %>%
    filter(group == 'b') %>% select(Group2 = group, Group2_rr = rr))

  test_pasting <- paste_tbl_grp(data = testing_stats_3digits)
  names(test_pasting) <- c('Comparison', 'ResponseStats')
  testing_results <- data.frame(test_pasting,
    ResponseTest  = two_samp_bin_test(x = x, y = group),
    PerfectSeparation = ifelse(diff(testing_stats_pre_3digits$mean) == 1,
                               TRUE, FALSE))
  expect_equal(object = pairwise_test_bin(x = x, group = group, digits = 3),
               expected = testing_results)


  # One-sided test comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, alternative = 'less')

  names(test_pasting) <- c('Comparison', 'ResponseStats')

  testing_results <- data.frame(test_pasting,
    # Need reverse testing direction
    ResponseTest  = two_samp_bin_test(x = x, y = group, alternative = 'greater'),
    PerfectSeparation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))

  expect_equal(object = pairwise_test_bin(x = x, group = group, alternative = 'less'),
               expected = testing_results)



  # sorted group greater than comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, alternative = 'greater',
                                first_name = 'Group2', second_name = 'Group1')
  names(test_pasting) <- c('Comparison', 'ResponseStats')
  testing_results <- data.frame(
    test_pasting,
    # Need reverse testing direction
    ResponseTest  = two_samp_bin_test(x = x, y = factor(group, levels = c('b','a')),
                                      alternative = 'less'),
    PerfectSeparation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))
  expect_equal(object = pairwise_test_bin(x = x, group = group, alternative = 'greater',
                                          sorted_group = c('b','a')),
               expected = testing_results)



  # High number needed for testing
  test_pasting <- paste_tbl_grp(data = testing_stats)
  names(test_pasting) <- c('Comparison', 'ResponseStats')
  testing_results <- data.frame(
    test_pasting,
    ResponseTest  = NA_integer_,
    PerfectSeparation = ifelse(all(testing_stats_pre$mean == 0) |
                                 all(testing_stats_pre$mean == 1),
                               TRUE, FALSE))
  expect_equal(object = pairwise_test_bin(x = x, group = group, num_needed_for_test = 100),
               expected = testing_results)


  # Paired data (mcnemar testing)
  paired_ids <- na.omit(test_data)$id[(duplicated(na.omit(test_data)$id))]

  paired_stats_pre <- test_data %>%
    filter(id %in% paired_ids) %>%
    group_by(group) %>%
    mutate(num_pos = sum(x), n = n()) %>%
    group_by(group,num_pos, n) %>%
    group_modify( ~ wilson_ci(.$x, .95)) %>% ungroup() %>%
    mutate(rr = paste0(num_pos, '/', n, ' = ', stat_paste(mean * 100,
                                                          lower * 100,
                                                          upper * 100,
                                                          digits = 1,
                                                          suffix = '%')))

  paired_stats <- bind_cols(paired_stats_pre %>%
                              filter(group == 'a') %>%
                              select(Group1 = group, Group1_rr = rr), paired_stats_pre %>%
                              filter(group == 'b') %>%
                              select(Group2 = group, Group2_rr = rr))


  test_pasting <- paste_tbl_grp(data = paired_stats)
  # this tests that the group order can be switched at the pairwise call
  test_pasting_rev <- paste_tbl_grp(data = paired_stats,
                                    first_name = "Group2",
                                    second_name = "Group1")
  expect_false(identical(test_pasting, test_pasting_rev))

  names(test_pasting) <- c('Comparison', 'ResponseStats')
  names(test_pasting_rev) <- c('Comparison', 'ResponseStats')

  testing_results <- data.frame(test_pasting,
    ResponseTest  = two_samp_bin_test(x = x, y = group, method = 'mcnemar'),
    PerfectSeparation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))

  expect_equal(object = pairwise_test_bin(x = x, group = group, id = id, method = 'mcnemar'),
               expected = testing_results)

  testing_results_rev <- data.frame(test_pasting_rev,
                                ResponseTest  = two_samp_bin_test(x = x, y = group, method = 'mcnemar'),
                                PerfectSeparation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))

  expect_equal(object = pairwise_test_bin(x = x,
                                          group = factor(group, levels = c("b", "a")),
                                          id = id, method = 'mcnemar'),
               expected = testing_results_rev)

})

# test pairwise_test_bin with 3+ groups.
# Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_test_bin testing 3+ groups", {
  library(tidyr)
  library(dplyr)
  library(VISCfunctions)
  data("exampleData_BAMA")


  testing_results <- exampleData_BAMA %>%
    filter(visitno != 0) %>%
    group_by(antigen, visitno, group) %>%
    summarise(rfraction = paste0(sum(response), "/", n()),
              ci = wilson_ci(response),
              r1 = sum(response),
              r0 = abs(sum(response - 1)),
              .groups = "keep")  %>%
    pivot_wider(id_cols = c(antigen, visitno),
                names_from = group,
                names_prefix = "grp",
                values_from = c(rfraction, ci, r0, r1)) %>%
    mutate(pval = ifelse(((r0_grp1 == 0 & r0_grp2 == 0) | (r1_grp1 == 0 & r1_grp2 == 0)),
                              1,
                              as.double(Exact::exact.test(matrix(c(r1_grp1,
                                                                   r1_grp2,
                                                                   r0_grp1,
                                                                   r0_grp2),
                                                                 2, 2, byrow = FALSE),
                                              method = "z-pooled",
                                              to.plot = FALSE,
                                              alternative = "two.sided")$p.value)),
           PerfectSeparation = FALSE,
           Comparison = "1 vs. 2",
           ResponseStats = paste0(rfraction_grp1,
                                  " = ",
                                  round_away_0(ci_grp1$mean*100, 1, trailing_zeros = TRUE),
                                  "% (",
                                  round_away_0(ci_grp1$lower*100, 1, trailing_zeros = TRUE),
                                  "%, ",
                                  round_away_0(ci_grp1$upper*100, 1, trailing_zeros = TRUE),
                                  "%) vs. ",
                                  rfraction_grp2,
                                  " = ",
                                  round_away_0(ci_grp2$mean*100, 1, trailing_zeros = TRUE),
                                  "% (",
                                  round_away_0(ci_grp2$lower*100, 1, trailing_zeros = TRUE),
                                  "%, ",
                                  round_away_0(ci_grp2$upper*100, 1, trailing_zeros = TRUE),
                                  "%)"))  %>%
    select(antigen, visitno, Comparison, ResponseStats, ResponseTest = pval, PerfectSeparation)

  function_obj <- exampleData_BAMA %>%
    group_by(antigen, visitno) %>%
    group_modify(~ as.data.frame(pairwise_test_bin(x = .$response,
                                                   group = .$group,
                                                   method = 'barnard',
                                                   barnard_method = "z-pooled",
                                                   num_needed_for_test = 2,
                                                   digits = 1,
                                                   trailing_zeros = TRUE,
                                                   sep_val = ' vs. ',
                                                   verbose = TRUE)))

  expect_equal(object = function_obj,
               expected = testing_results)

})




# Pairwise Correlation Testing --------------------------------------------


test_that("error checking", {
  library(tidyr)
  library(dplyr)
  set.seed(243542534)
  x = c(NA, rnorm(25, 0, 1), rnorm(25, 1, 1),NA)
  pair = c(rep('a', 26),rep('b', 26))
  id = c(1:26, 1:26)

  test_data <- data.frame(x, pair, id)
  test_data_wide <- test_data %>%
    pivot_wider(names_from = pair, values_from = x) %>%
    drop_na()

  expect_error(
    object =   cor_test_pairs(x = x,
                                 pair = pair,
                                 id = 1:10),
    regexp = '"x", "pair", and "id" must be same length'
  )

  expect_error(
    object =   cor_test_pairs(x = x,
                                 pair = pair,
                                 id = id,
                                 n_distinct_value = 1),
    regexp = '"n_distinct_value" must be >1'
  )

  expect_error(
    object =   cor_test_pairs(x = rep(1:2, 26),
                                 pair = pair,
                                 id = id),
    regexp = 'All pairs have less than 3 distinct values'
  )

  expect_error(
    object =   cor_test_pairs(x = c(rep(1:2, 25), 3, 4),
                                 pair = pair,
                                 id = id),
    regexp = 'Only one pair has >=3 distinct values, so no testing possible'
  )

  expect_message(
    object = cor_test_pairs(x = c(NA,NA,NA,x[-(1:3)],1,2,3),
                                   pair = c(pair,rep('c',3)),
                                   id = c(id,1:3), verbose = TRUE),
    regexp = 'No non-missing data points when considering a and c'
  )




})



# test cor_test_pairs Using paste_tbl_grp and cor_test in testing since these functions are testing elsewhere
test_that("cor_test_pairs testing two groups", {
  library(tidyr)
  library(dplyr)
  set.seed(243542534)
  x = c(NA, rnorm(25, 0, 1), rnorm(25, 1, 1),NA, 1)
  group = factor(c(rep('a', 26),rep('b', 26), 'c'))
  id = c(1:26, 1:26, 1)

  test_data <- data.frame(x, group, id)
  test_data_wide <- test_data[-length(x),] %>%
    pivot_wider(names_from = group, values_from = x) %>%
    drop_na()


  testing_results <- data.frame(
    Correlation = 'a and b',
    NPoints = nrow(test_data_wide),
    Ties = case_when(
      any(duplicated(test_data_wide$a)) &
        any(duplicated(test_data_wide$b)) ~ 'ties in both',
      any(duplicated(test_data_wide$a)) ~ 'ties in a',
      any(duplicated(test_data_wide$b)) ~ 'ties in b',
      TRUE ~ 'no ties'
    ),
    CorrEst = stats::cor(x = test_data_wide$a,
                       y = test_data_wide$b,
                       method = 'spearman', use = 'pairwise.complete.obs') %>%
      round_away_0(digits = 3, trailing_zeros = TRUE),
    CorrTest = cor_test(x = test_data_wide$a,
                        y = test_data_wide$b,
                        method = 'spearman'),
    stringsAsFactors = FALSE
  )

  expect_equal(object = cor_test_pairs(x = test_data$x,
                                       pair = test_data$group,
                                       id = test_data$id,
                                       method = 'spearman',
                                       n_distinct_value = 3,
                                       digits = 3,
                                       trailing_zeros = TRUE,
                                       verbose = FALSE),
               expected = testing_results)

  expect_message(
    object = cor_test_pairs(x = test_data$x,
                            pair = test_data$group,
                            id = test_data$id,
                            method = 'spearman',
                            n_distinct_value = 3,
                            digits = 3,
                            trailing_zeros = TRUE,
                            verbose = TRUE),
    regexp = 'Pair\\(s\\) c are excluded because the distinct values are less than 3'
  )

  # Digits to 5
  testing_results5 <- testing_results
  testing_results5$CorrEst <- stats::cor(x = test_data_wide$a,
                                        y = test_data_wide$b,
                                        method = 'spearman',
                                        use = 'pairwise.complete.obs') %>%
    round_away_0(digits = 5, trailing_zeros = TRUE)

  expect_equal(object = cor_test_pairs(x = test_data$x,
                                             pair = test_data$group,
                                             id = test_data$id,
                                             method = 'spearman',
                                             n_distinct_value = 3,
                                             digits = 5,
                                             trailing_zeros = TRUE,
                                             verbose = FALSE),
               expected = testing_results5)
})




# test cor_test_pairs Using paste_tbl_grp and cor_test in testing since these functions are testing elsewhere
test_that("cor_test_pairs testing multiple groups", {
  library(tidyr)
  library(dplyr)

  test_single_comp <- function(data_in) {

    test_data <- data_in %>%
      filter(!is.na(x)) %>%
      mutate(group = droplevels(group))

    test_data_wide <- test_data %>%
      pivot_wider(names_from = group, values_from = x) %>%
      drop_na()

    testing_results <- data.frame(
      Correlation = paste0(levels(test_data$group)[1],
                          ' and ',
                          levels(test_data$group)[2]),
      NPoints = nrow(test_data_wide),
      Ties = case_when(
        any(duplicated(test_data_wide[,2, drop = TRUE])) &
          any(duplicated(test_data_wide[,3, drop = TRUE])) ~ 'ties in both',
        any(duplicated(test_data_wide[,2, drop = TRUE])) ~
          paste0('ties in ', levels(test_data$group)[1]),
        any(duplicated(test_data_wide[,3, drop = TRUE])) ~
          paste0('ties in ', levels(test_data$group)[2]),
        TRUE ~ 'no ties'
      ),
      CorrEst = ifelse(sum(!test_data_wide[,2, drop = TRUE] %>% duplicated) >= 3 &
                         sum(!test_data_wide[,3, drop = TRUE] %>% duplicated) >= 3,
        stats::cor(x = test_data_wide[,2, drop = TRUE],
                           y = test_data_wide[,3, drop = TRUE],
                           method = 'spearman', use = 'pairwise.complete.obs') %>%
        round_away_0(digits = 3, trailing_zeros = TRUE),
        NA),
      CorrTest = cor_test(x = test_data_wide[,2, drop = TRUE],
                          y = test_data_wide[,3, drop = TRUE],
                          method = 'spearman'),
      stringsAsFactors = FALSE
    )
    testing_results
  }

  test_all_comp <- function(x, group, id){
    x_here <- x[!is.na(x)]
    group_here <- droplevels(factor(group[!is.na(x)]))
    id_here <- id[!is.na(x)]
    if (length(unique(group[!is.na(x)])) > 1) {
      test_results_paste <- list()
      for (i in 1:(nlevels(group_here) - 1)) {
        for (j in ((i + 1):nlevels(group_here))) {
          tmp_index <- group_here %in% levels(group_here)[c(i,j)]
          test_results_paste[[length(test_results_paste) + 1]] <-
            test_single_comp(data_in = data.frame(
              x = x_here[tmp_index],
              group = group_here[tmp_index],
              id = id_here[tmp_index]))
        }
      }
      do.call(base::rbind, test_results_paste)
    }
  }

  testing_results <- testData_BAMA %>%
    group_by(group, visit) %>%
    group_modify(~test_all_comp(x = .x$magnitude,
                                group = .x$antigen,
                                id = .x$pubID) %>%
                   as.data.frame)

  group_testing_dt <- testData_BAMA %>%
    group_by(group, visit) %>%
    group_modify(~cor_test_pairs(x = .x$magnitude,
                                    pair = .x$antigen,
                                    id = .x$pubID,
                                    method = 'spearman',
                                    n_distinct_value = 3,
                                    digits = 3,
                                    trailing_zeros = TRUE,
                                    verbose = FALSE) %>%
                   as.data.frame)

  expect_equal(object = group_testing_dt,
               expected = testing_results)

  expect_message(
    object = testData_BAMA %>%
      group_by(group, visit) %>%
      group_modify(~cor_test_pairs(x = .x$magnitude,
                                   pair = .x$antigen,
                                   id = .x$pubID,
                                   method = 'spearman',
                                   n_distinct_value = 3,
                                   digits = 3,
                                   trailing_zeros = TRUE,
                                   verbose = TRUE)),
    regexp = 'Not enough distinct values for at least one pair when considering Con S gp140 CFI and gp70_C.1086C V1/V2/293F'
  )

})


