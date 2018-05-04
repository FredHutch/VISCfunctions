context("pretty_output_functions")


# test tbl_grp_paste
test_that("tbl_grp_paste testing various options (no errors)", {


  ## Creating Testing Dataset for Pasting
  require(VISCfunctions.data)
  data(exampleData_BAMA)
  testing_dataset <- exampleData_BAMA[, .(
    Group1 = unique(group[group == 1]), Group2 = unique(group[group == 2]),
    Group1_n = length(magnitude[group == 1]), Group2_n = length(magnitude[group == 2]),
    Group1_mean = mean(magnitude[group == 1]), Group2_mean = mean(magnitude[group == 2]),
    Group1_sd = sd(magnitude[group == 1]), Group2_sd = sd(magnitude[group == 2]),
    Group1_median = median(magnitude[group == 1]), Group2_median = median(magnitude[group == 2]),
    Group1_min = min(magnitude[group == 1]), Group2_min = min(magnitude[group == 2]),
    Group1_max = max(magnitude[group == 1]), Group2_max = max(magnitude[group == 2]),
    Group1_IQR = IQR(magnitude[group == 1])
  ), by = .(visitno,antigen)]

  testing_fun <- function(data_in, first_sep, sep, digits) {
    data.frame(visitno = testing_dataset$visitno,
               antigen = testing_dataset$antigen,
               Group1_IQR = testing_dataset$Group1_IQR,
               Comparison = paste0(testing_dataset$Group1, first_sep, testing_dataset$Group2),
               n_comparison = paste0(testing_dataset$Group1_n, sep, testing_dataset$Group1_n),
               mean_comparison = paste0(round_away_0(testing_dataset$Group1_mean, digits), sep, round_away_0(testing_dataset$Group2_mean, digits)),
               sd_comparison = paste0(round_away_0(testing_dataset$Group1_sd, digits), sep, round_away_0(testing_dataset$Group2_sd, digits)),
               median_comparison = paste0(round_away_0(testing_dataset$Group1_median, digits), sep, round_away_0(testing_dataset$Group2_median, digits)),
               min_comparison = paste0(round_away_0(testing_dataset$Group1_min, digits), sep, round_away_0(testing_dataset$Group2_min, digits)),
               max_comparison = paste0(round_away_0(testing_dataset$Group1_max, digits), sep, round_away_0(testing_dataset$Group2_max, digits)),
               median_min_max_comparison = paste0(
                 paste0(round_away_0(testing_dataset$Group1_median, digits), '[',
                        round_away_0(testing_dataset$Group1_min, digits), ', ',
                        round_away_0(testing_dataset$Group1_max, digits), ']', sep = ''),
                 sep,
                 paste0(round_away_0(testing_dataset$Group2_median, digits), '[',
                        round_away_0(testing_dataset$Group2_min, digits), ', ',
                        round_away_0(testing_dataset$Group2_max, digits), ']', sep = '')
               ),
               mean_sd_comparison = paste0(
                 paste0(round_away_0(testing_dataset$Group1_mean, digits), '(',
                        round_away_0(testing_dataset$Group1_sd, digits), ')', sep = ''),
                 sep,
                 paste0(round_away_0(testing_dataset$Group2_mean, digits), '(',
                        round_away_0(testing_dataset$Group2_sd, digits), ')', sep = '')
               ),
               stringsAsFactors = FALSE)
  }


  # Testing
  default_expected_results <- testing_fun(data_in = testing_dataset,first_sep = ' vs. ', sep = ' vs. ',digits = 0)
  expect_equal(object = tbl_grp_paste(data = testing_dataset),
               expected = default_expected_results
  )
  # Testing message with all
  expect_message(object = tbl_grp_paste(data = testing_dataset, verbose = TRUE),
                 regexp = 'The following measures will be combined: n, mean, sd, median, min, max, median_min_max, mean_sd'
  )
  # No Passthrough var
  expect_equal(object = tbl_grp_paste(data = testing_dataset, keep_all = FALSE),
               expected = default_expected_results[, !names(default_expected_results) %in% c('visitno','antigen','Group1_IQR')]
  )
  # Different Seperator
  expect_equal(object = tbl_grp_paste(data = testing_dataset, sep_val = '/'),
               expected =  testing_fun(data_in = testing_dataset,first_sep = '/', sep = '/',digits = 0)
  )
  # Different Alt
  expect_equal(object = tbl_grp_paste(data = testing_dataset, alternative = 'less'),
               expected =  testing_fun(data_in = testing_dataset,first_sep = ' < ', sep = ' vs. ',digits = 0)
  )
  # Different Rounding Digits
  expect_equal(object = tbl_grp_paste(data = testing_dataset, digits = 5),
               expected =  testing_fun(data_in = testing_dataset,first_sep = ' vs. ', sep = ' vs. ',digits = 5)
  )
  # If all selected but no matching gives NULL or data, depening on keep_all, and gives message
  expect_equal(object = tbl_grp_paste(data = testing_dataset[, c('Group1','Group2','Group1_mean','Group2_sd')]),
               expected =  testing_dataset[, c('Group1','Group2','Group1_mean','Group2_sd')]
  )
  expect_message(object = tbl_grp_paste(data = testing_dataset[, c('Group1','Group2','Group1_mean','Group2_sd')], verbose = TRUE),
               regexp =  '"all" specified, but no matching columns to paste'
  )
  expect_null(object = tbl_grp_paste(data = testing_dataset[, c('Group1','Group2','Group1_mean','Group2_sd')], keep_all = FALSE))
  # Trying different group naming
  expect_equal(object = tbl_grp_paste(data = testing_dataset[, .(Group_1 = Group1, Group_2 = Group2, Group_2_mean = Group2_mean, Group_1_mean = Group1_mean)], first_name = 'Group_1', second_name = 'Group_2'),
               expected =  default_expected_results[, c('Comparison', 'mean_comparison')]
  )
  expect_equal(object = tbl_grp_paste(data = testing_dataset[, .(`G.r/o|up_1` = Group1, Group_2 = Group2, Group_2_mean = Group2_mean, `G.r/o|up_1_mean` = Group1_mean)], first_name = 'G.r/o|up_1', second_name = 'Group_2', keep_all = FALSE),
               expected =  default_expected_results[, c('Comparison', 'mean_comparison')]
  )


  ### Throwing errors
  # Wrong Group names
  expect_error(object = tbl_grp_paste(data = testing_dataset, first_name = 'Group3'),
               regexp = 'Expecting one column named "Group3" in input dataset, but there are 0 present'
  )
  expect_error(object = tbl_grp_paste(data = testing_dataset, second_name = 'Group3'),
               regexp = 'Expecting one column named "Group3" in input dataset, but there are 0 present'
  )
  expect_error(object = tbl_grp_paste(data = testing_dataset[, !names(testing_dataset) %like% 'Group1']),
               regexp = 'Expecting one column named "Group1" in input dataset, but there are 0 present'
  )

  # Wrong Measures Given or no matching measures
  expect_error(object = tbl_grp_paste(data = testing_dataset[, c('Group1','Group2','Group1_mean','Group2_mean')], vars_to_paste = 'mean_sd'),
               regexp = 'Expecting one column named "Group1_sd" in input dataset, but there are 0 present'
  )


})


# test stat_paste
test_that("stat_paste testing various options (no errors)", {

  expect_equal(object = stat_paste(5.109293), expected = '5')
  expect_equal(object = stat_paste(5.109293, digits = 2), expected = '5.11')
  expect_equal(object = stat_paste(NA), expected = '---')
  expect_equal(object = stat_paste(5.109293, 2.145, digits = 2), expected = "5.11 (2.15)")
  expect_equal(object = stat_paste(5.109293, 2.145, digits = 2, bound_char = '['), expected = "5.11 [2.15]")
  expect_equal(object = stat_paste(5.109293, 2.145, digits = 2, bound_char = '{'), expected = "5.11 {2.15}")
  expect_equal(object = stat_paste(5.109293, 2.145, digits = 2, bound_char = '|'), expected = "5.11 |2.15|")
  expect_equal(object = stat_paste(5.109293, 2.145, 8.0345, digits = 2), expected = "5.11 (2.15, 8.03)")
  expect_equal(object = stat_paste(5.109293, 2.145, 8.0345, digits = 2, sep = '---'), expected = "5.11 (2.15---8.03)")
  expect_equal(object = stat_paste(NA, NA, NA), expected = '---')
  expect_equal(object = stat_paste(NA, NA, NA, na_str_out = 'NA'), expected = 'NA')
  expect_equal(object = stat_paste(5.109, "p < 0.001", digits = 3), expected = "5.109 (p < 0.001)")
  expect_equal(object = stat_paste(c(rep(5,5),NA),c(1:5,NA),c(1,NA,2,NA,3,NA), bound_char = '['),
               expected = c("5 [1, 1]", "5 [2, ---]", "5 [3, 2]", "5 [4, ---]", "5 [5, 3]", "---"  ))

})
