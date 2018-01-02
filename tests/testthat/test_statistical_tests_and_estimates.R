context("statistical_tests_and_estimates")

# test two_samp_cont_test
test_that("two_samp_cont_test testing various options (no errors)", {

  ###Testing all four options (wilcox/t and paired/unpaired)###
  set.seed(5432322)
  x <- c(NA,rnorm(10,0,3), rnorm(10,3,3),NA)
  y <- c(rep('a', 10), NA, NA, rep('b', 10))

  # Wilcox Unpaired
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 'wilcox', paired = FALSE, verbose = T),
               expected = coin::pvalue(coin::wilcox_test(x~factor(y),distribution = "exact", ties.method = "mid-ranks"))
               , tolerance = 1e-8)
  # Wilcox Paired
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 'wilcox', paired = TRUE, verbose = T),
               expected = coin::pvalue(coin::wilcoxsign_test(x[1:10]~x[13:22],distribution = "exact"))
               , tolerance = 1e-8)
  # T-Test Unpaired
  #t.test(x[1:10], x[13:22], paired=F, var.equal = F)$p.value
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T),
               expected = t.test(x~factor(y), paired = F, var.equal = F)$p.value,
               tolerance = 1e-8)
  # T-Test Paired
  #Note the list also works: t.test(x[1:10], x[13:22], paired=T, var.equal = F)$p.value
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 't', paired = TRUE, verbose = T),
               expected = t.test(x[-c(1,10:13,22)]~factor(y[-c(1,10:13,22)]), paired = T, var.equal = F)$p.value,
               tolerance = 1e-8)
  #Testing var.equal = T option in T-Test Unpaired
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T, var.equal = T),
               expected = t.test(x~factor(y), paired = F, var.equal = T)$p.value,
               tolerance = 1e-8)
  #Testing alternative param can be used
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T, alternative = 'less'),
               expected = t.test(x~factor(y), paired = F, alternative = 'less')$p.value,
               tolerance = 1e-8)

  #Testing t.test where both levels of y have a single x value
  expect_equal(object = two_samp_cont_test(x = rep(1:2,5), y = rep(0:1,5), method = 't'), expected = NA)
  expect_message(object = two_samp_cont_test(x = rep(1:2,5), y = rep(0:1,5), method = 't', verbose = T),
                 regexp = 't.test can not run when both levels of "y" have only 1 unique "x" value, so p=NA returned')


})


test_that("two_samp_cont_test throwing internal .rm_na_and_check checking errors", {
  set.seed(5432322)
  x <- c(NA,rnorm(10,0,3), rnorm(10,3,3),NA)
  y <- c(rep('a', 10), NA, NA, rep('b', 10))

  #Testing if x and y are different lengths
  expect_error(two_samp_cont_test(x = 1:9, y = rep(0:1,5)), '"x" and "y" must be the same length')

  #Testing case where no non-missing pairs
  expect_equal(object = two_samp_cont_test(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20))), expected = NA)
  expect_message(object = two_samp_cont_test(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20)), verbose = T),
                 regexp = 'There are no observations with non-missing values of both "x" and "y", so p=NA returned')

  #Testing case where all x have same value
  expect_equal(object = two_samp_cont_test(x = rep(1,22), y = y), expected = 1)
  expect_message(object = two_samp_cont_test(x = rep(1,22), y = y, verbose = T),
                 regexp = '"x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned')

  #Testing case where all y have same value
  expect_equal(object = two_samp_cont_test(x = x, y = rep(1,22)), expected = NA)
  expect_message(object = two_samp_cont_test(x = x, y = rep(1,22), verbose = T),
                 regexp = '"y" only has 1 level when considering non-missing values of "x", so p=NA returned')
})

test_that("two_samp_cont_test throwing internal input checking errors", {
  set.seed(5432322)
  x <- c(rnorm(10,0,3), rnorm(10,3,3))
  y <- c(rep('a', 10), rep('b', 10))
  my_matrix <- matrix(1:10,nrow = 2)

  #Checking x
  expect_error(two_samp_cont_test(my_matrix, y = y), '"x" must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_cont_test(x = numeric(0), y = y), '"x" length must be > 0')
  expect_error(two_samp_cont_test(c(NA,NA,NA), y), '"x" must have at least one non-NA value')
  expect_error(two_samp_cont_test(letters[1:5],y), '"x" must be a numeric vector')

  #Checking y
  expect_error(two_samp_cont_test(x, my_matrix), '"y" must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_cont_test(x,numeric(0)), '"y" length must be > 0')
  expect_error(two_samp_cont_test(x,c(NA,NA,NA)), '"y" must have at least one non-NA value')
  expect_error(two_samp_cont_test(x,1:10), '"y" cannot have more than 2 distinct values')

  #Testing paired errors
  expect_error(two_samp_cont_test(x = 1:10, y = c(rep(0:1,4),0,NA), method = 't', paired = TRUE), 'When "paired" = TRUE "y" cannot have missing values')
  expect_error(two_samp_cont_test(x = 1:11, y = c(rep(0:1,5),1), paired = TRUE), 'When "paired" = TRUE "y" must have the same number of samples for each level')
})
