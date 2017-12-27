context("statistical_tests_and_estimates")

# test two_samp_cont_test
test_that("two_samp_cont_test testing", {

  ###Testing all four options (wilcox/t and paired/unpaired)###
  set.seed(5432322)
  x <- c(NA,rnorm(10,0,3), rnorm(10,3,3),NA)
  y <- c(rep('a', 10), NA, NA, rep('b', 10))

  #coin::pvalue(coin::wilcox_test(x~factor(y),distribution = "exact", ties.method = "mid-ranks"))
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 'wilcox', paired = FALSE, verbose = T),
               expected = 0.02443439, tolerance = 1e-8)
  #coin::pvalue(coin::wilcoxsign_test(x[1:10]~x[13:22],distribution = "exact"))
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 'wilcox', paired = TRUE, verbose = T),
               expected = 0.109375, tolerance = 1e-8)
  #t.test(x~factor(y), paired=F, var.equal = F)$p.value
  #t.test(x[1:10], x[13:22], paired=F, var.equal = F)$p.value
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T),
               expected = 0.02138081, tolerance = 1e-8)
  #t.test(x[-c(1,10:13,22)]~factor(y[-c(1,10:13,22)]), paired=T, var.equal = F)$p.value
  #t.test(x[1:10], x[13:22], paired=T, var.equal = F)$p.value
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 't', paired = TRUE, verbose = T),
               expected = 0.0752293, tolerance = 1e-8)
  #Testing var.equal = T option in t.test
  #t.test(x~factor(y), paired=F, var.equal = T)$p.value
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T, var.equal = T),
               expected = 0.02129004, tolerance = 1e-8)

  #Testing if x and y are different lengths
  expect_error(two_samp_cont_test(x = x[-1], y = y), '"x" and "y" must be the same length')

  #Testing paired errors
  expect_error(two_samp_cont_test(x = x, y = y, method = 't', paired = TRUE), 'When "paired" = TRUE "y" can not have missing values')
  expect_error(two_samp_cont_test(x = x[-(11:13)], y = y[-(11:13)], method = 't', paired = TRUE), 'When "paired" = TRUE "y" must have the same number of samples for each level')

  #Testing case where no non-missing pairs
  expect_equal(object = two_samp_cont_test(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20))), expected = NA)
  expect_message(object = two_samp_cont_test(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20)), verbose = T),
                 regexp = 'There are no observations with non-mising values of both "x" and "y", so p=NA returned')

  #Testing case where all x have same value
  expect_equal(object = two_samp_cont_test(x = rep(1,22), y = y), expected = 1)
  expect_message(object = two_samp_cont_test(x = rep(1,22), y = y, verbose = T),
                 regexp = '"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')

  #Testing case where all y have same value
  expect_equal(object = two_samp_cont_test(x = x, y = rep(1,22)), expected = NA)
  expect_message(object = two_samp_cont_test(x = x, y = rep(1,22), verbose = T),
                 regexp = '"y" only has 1 level when considering non-missing values of x, so p=NA returned')
})

test_that("two_samp_cont_test throwing internal input checking errors", {
  set.seed(5432322)
  x <- c(rnorm(10,0,3), rnorm(10,3,3))
  y <- c(rep('a', 10), rep('b', 10))
  my_matrix <- matrix(1:10,nrow = 2)

  #Checking x
  expect_error(two_samp_cont_test(my_matrix, y = y), 'x must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_cont_test(x = numeric(0), y = y), 'x length must be > 0')
  expect_error(two_samp_cont_test(c(NA,NA,NA), y), 'x must have at least a non "NA" value')
  expect_error(two_samp_cont_test(letters[1:5],y), 'x must be a numeric vector')

  #Checking y
  expect_error(two_samp_cont_test(x, my_matrix), 'y must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_cont_test(x,numeric(0)), 'y length must be > 0')
  expect_error(two_samp_cont_test(x,c(NA,NA,NA)), 'y must have at least a non "NA" value')
  expect_error(two_samp_cont_test(x,1:10), 'y can not have more than 2 distinct values')
})
