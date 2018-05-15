context("pretty_output_functions")


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

# test pretty_pvalues
test_that("pretty_pvalues testing various options (no errors)", {
  
  expect_equal(object = pretty_pvalues(0.00000001), expected = '<0.001')
  expect_equal(object = pretty_pvalues(c(0.00000001, NA, 0.05), digits = 2, missing_char = "missing"), expected = c("<0.01",   "missing", "0.05"))
  expect_equal(object = pretty_pvalues(c(0.00000001, NA, 0.05, 1), digits = 3, trailing_zeros = T, bold = T, background = "pink"), expected = c("\\cellcolor{pink}{\\textbf{<0.001}}", "---", "\\cellcolor{pink}{\\textbf{0.050}}", "1.000"))
  expect_equal(object = pretty_pvalues(c(NaN, .5), trailing_zeros = FALSE), expected = c('---', "0.5"))
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