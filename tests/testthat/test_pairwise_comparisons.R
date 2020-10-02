context("pairwise_comparisons")


# test pairwise_test_cont. Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons_bin testing two groups", {
  library(tidyr)
  library(dplyr)
  set.seed(243542534)
  x = c(NA, rnorm(25, 0, 1), rnorm(25, 1, 1),NA)
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
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
                                PerfectSeperation = ifelse((testing_stats$Group1_min >  testing_stats$Group2_max) |
                                                             (testing_stats$Group2_min >  testing_stats$Group1_max),
                                                           TRUE, FALSE))
  expect_equal(object = test_data %>% pairwise_test_cont(x = x, group = group,
                                           paired = FALSE, method = 'wilcox',
                                           alternative = 'two.sided', num_needed_for_test = 100,
                                           digits = 3, trailing_zeros = TRUE,
                                           sep_val = ' vs. ', verbose = FALSE),
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

  testing_results <- data.frame(Comparison = test_pasting$Comparison,
                                SampleSizes =  sum(duplicated(na.omit(data.frame(x, group, id))$id)),
                                Median_Min_Max = test_pasting$median_min_max_comparison,
                                Mean_SD = test_pasting$mean_sd_comparison,
                                MagnitudeTest = two_samp_cont_test(x = x, y = group, method = 'wilcox', paired = TRUE),
                                PerfectSeperation = ifelse((testing_stats_paired$Group1_min >
                                                              testing_stats_paired$Group2_max) |
                                                             (testing_stats_paired$Group2_min >
                                                                testing_stats_paired$Group1_max), TRUE, FALSE),
                                stringsAsFactors = FALSE)

  expect_equal(object = test_ids %>% filter() %>%  pairwise_test_cont(x = x, group = group,
                                           paired = TRUE, id = id, sorted_group = c("a", "b"),
                                           method = 'wilcox', alternative = 'two.sided',
                                           num_needed_for_test = 3, digits = 3,
                                           trailing_zeros = TRUE, sep_val = ' vs. ',
                                           verbose = FALSE),
               expected = testing_results)

})

# test pairwise_test_cont. Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons testing multiple groups", {
  library(tidyr)
  library(purrr)
  pvals <-
    exampleData_BAMA %>%
    pivot_wider(id_cols = c(antigen, visitno),
                names_from = group,
                values_from = c(magnitude, response),
                values_fn = list) %>%
    map2(.x = .$magnitude_1, .y = .$magnitude_2, .f = ~wilcox.test(x=.x, y=.y)) %>% map_dbl("p.value")

  testing_results <-
    exampleData_BAMA %>%
    group_by(antigen, visitno, group) %>%
    summarise(n = n(),
              mean = mean(magnitude, na.rm = TRUE),
              sd = sd(magnitude, na.rm = TRUE),
              median = median(magnitude, na.rm = TRUE),
              min = min(magnitude, na.rm = TRUE),
              max = max(magnitude, na.rm = TRUE),
              .groups = "keep") %>%
    pivot_wider(names_from = group,
                values_from = c(n, mean, sd, median, min, max)) %>%
    mutate(Group1 = "1", Group2 = "2", .before = "n_1") %>%
    cbind(MagnitudeTest = pvals) %>%
    mutate(PerfectSeperation = ifelse(min_1 > max_2| max_2 < min_1, TRUE, FALSE), .after = "max_2")

  colnames(testing_results)[5:16] <- c("Group1_n", "Group2_n", "Group1_mean",
                                     "Group2_mean", "Group1_sd", "Group2_sd",
                                     "Group1_median", "Group2_median",
                                     "Group1_min", "Group2_min", "Group1_max",
                                     "Group2_max")

  test_pasting <- paste_tbl_grp(data = testing_results,
                                vars_to_paste = c('n','median_min_max', 'mean_sd'),
                                sep_val = " vs. ",
                                digits = 3,
                                keep_all = TRUE,
                                trailing_zeros = TRUE)
  names(test_pasting)[5:8] <- c('Comparison', 'SampleSizes', 'Median_Min_Max', 'Mean_SD')
  test_pasting <- test_pasting %>% select(antigen,
                                          visitno,
                                          Comparison,
                                          SampleSizes,
                                          Median_Min_Max,
                                          Mean_SD,
                                          MagnitudeTest,
                                          PerfectSeperation)

  group_testing_dt <- exampleData_BAMA %>%
    group_by(antigen, visitno) %>%
    summarise(pairwise_test_cont(x = magnitude,
                                 group = group,
                                 paired = FALSE,
                                 method = 'wilcox',
                                 alternative = 'two.sided', sorted_group = 1:2,
                                 num_needed_for_test = 3,
                                 digits = 0,
                                 trailing_zeros = TRUE,
                                 sep_val = ' vs. ',
                                 verbose = FALSE),
              .groups = "keep")

  expect_equal(object = group_testing_dt$antigen,
               expected = test_pasting$antigen)
})

test_that("Test example with fixed result", {
  source("fixed_data.R")
  data(exampleData_BAMA)

  group_testing_dt <- exampleData_BAMA[, pairwise_test_cont(
     x = magnitude, group = group, paired = FALSE, method = 'wilcox',
     alternative = 'less', num_needed_for_test = 3, digits = 3,
     trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE
    ),
    by = .(antigen, visitno)][order(antigen, visitno)]

  expect_equal(object = group_testing_dt,
               expected = fixed_bama_group_testing_dt)
  })


test_that("Paired results with test data", {
  library(dplyr)

  paired_example = subset(testData_BAMA, visit %in% c(1, 2) & antigen == "1086C_D7gp120.avi/293F")
  paired_example_subset1 = subset(paired_example,  visit == 1, select = -response)
  paired_example_subset2 = subset(paired_example,  visit == 2, select = -response)
  paired_data = merge(paired_example_subset1, paired_example_subset2, by = c("pubID", "antigen", "group"))

  paired_tests_ls = list()
  for (i in 1:length(unique(paired_data$group))) {
    temp_dat = subset(paired_data, group == unique(paired_data$group)[i])

    paired_tests_ls[[i]] =
      data.frame(
        group = unique(paired_data$group)[i],
        total = nrow(na.omit(temp_dat)),
        test = as.numeric(coin::pvalue(coin::wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y, distribution = "exact", zero.method = "Pratt"))),
        est_less =  as.numeric(coin::pvalue(coin::wilcoxsign_test(temp_dat$magnitude.x ~ temp_dat$magnitude.y, distribution = "exact", zero.method = "Pratt", alternative = "less")))
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

})


# test pairwise_test_bin.
#Using paste_tbl_grp and two_samp_bin_test in testing since these functions are testing elsewhere
test_that("pairwise_comparisons_bin testing two groups", {
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

  purrr::walk(c("barnard", "fisher", "chi.sq"),
              function(method_in){
                testing_results <- data.frame(test_pasting,
                                              ResponseTest  = two_samp_bin_test(x = x,
                                                                                y = group,
                                                                                method = method_in),
                                              PerfectSeperation = ifelse(diff(testing_stats_pre$mean) == 1,
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
    PerfectSeperation = ifelse(diff(testing_stats_pre_3digits$mean) == 1,
                               TRUE, FALSE))
  expect_equal(object = pairwise_test_bin(x = x, group = group, digits = 3),
               expected = testing_results)


  # One-sided test comparison
  test_pasting <- paste_tbl_grp(data = testing_stats, alternative = 'less')

  names(test_pasting) <- c('Comparison', 'ResponseStats')

  testing_results <- data.frame(test_pasting,
    # Need reverse testing direction
    ResponseTest  = two_samp_bin_test(x = x, y = group, alternative = 'greater'),
    PerfectSeperation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))

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
    PerfectSeperation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))
  expect_equal(object = pairwise_test_bin(x = x, group = group, alternative = 'greater',
                                          sorted_group = c('b','a')),
               expected = testing_results)



  # High number needed for testing
  test_pasting <- paste_tbl_grp(data = testing_stats)
  names(test_pasting) <- c('Comparison', 'ResponseStats')
  testing_results <- data.frame(
    test_pasting,
    ResponseTest  = NA_integer_,
    PerfectSeperation = ifelse(all(testing_stats_pre$mean == 0) |
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

  names(test_pasting) <- c('Comparison', 'ResponseStats')

  testing_results <- data.frame(test_pasting,
    ResponseTest  = two_samp_bin_test(x = x, y = group, method = 'mcnemar'),
    PerfectSeperation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE))

  expect_equal(object = pairwise_test_bin(x = x, group = group, id = id, method = 'mcnemar'),
               expected = testing_results)
})

# test pairwise_test_bin with 3+ groups.
# Using paste_tbl_grp and two_samp_cont_test in testing since these functions are testing elsewhere
test_that("pairwise_test_bin testing 3+ groups", {

  test_single_comp <- function(x, group, Group1, Group2) {
    test_data <- tibble(x, group) %>% filter(group %in% c(Group1, Group2))

    testing_stats_pre <- test_data %>%
      filter(!is.na(x)) %>%
      group_by(group) %>%
      mutate(num_pos = sum(x), n = n()) %>%
      group_by(group,num_pos, n) %>%
      group_modify( ~ wilson_ci(.$x, .95)) %>% ungroup() %>%
      mutate(rr = paste0(num_pos, '/', n, ' = ',
                         stat_paste(mean * 100, lower * 100, upper * 100, digits = 1, suffix = '%')))

    testing_stats <- bind_cols(
      testing_stats_pre %>% filter(group == Group1) %>% select(Group1 = group, Group1_rr = rr),
      testing_stats_pre %>% filter(group == Group2) %>% select(Group2 = group, Group2_rr = rr)
    )

    test_pasting <- paste_tbl_grp(data = testing_stats)
    names(test_pasting) <- c('Comparison', 'ResponseStats')

    data.frame(
      test_pasting,
      ResponseTest  = two_samp_bin_test(x = test_data$x, y = test_data$group),
      PerfectSeperation = ifelse(diff(testing_stats_pre$mean) == 1, TRUE, FALSE)
    )
  }

  test_all_comp <- function(x, group){
    x_here <- x[!is.na(x)]
    group_here <- droplevels(group[!is.na(x)])
    if (length(unique(group[!is.na(x)])) > 1) {
      test_results_paste <- list()
      for (i in 1:(nlevels(group_here) - 1)) {
        for (j in ((i + 1):nlevels(group_here))) {
          test_results_paste[[length(test_results_paste) + 1]] <-
            test_single_comp(x = x_here, group = group_here,
                             Group1 = levels(group_here)[i], Group2 = levels(group_here)[j])
        }
      }
      do.call(base::rbind, test_results_paste)
    }
  }

  testing_results <- testData_BAMA %>%
    group_by(antigen, visit) %>%
    group_modify(~ test_all_comp(.x$response, .x$group))

  function_obj <- testData_BAMA %>%
    group_by(antigen, visit) %>%
    group_modify(~ pairwise_test_bin(.x$response, .x$group, num_needed_for_test = 2))

  expect_equal(object = function_obj,
               expected = testing_results)

})

