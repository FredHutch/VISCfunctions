context("statistical_tests_and_estimates")

# test cont_vs_binary
test_that("cont_vs_binary testing", {

  ###No ties testing all four options (wilcox/t and paired/unpaired)###
  set.seed(5432322)
  x <- c(rnorm(10,0,3), rnorm(10,3,3))
  y <- c(rep('a', 10), rep('b', 10))

  #coin::pvalue(coin::wilcox_test(x~factor(y),distribution = "exact", ties.method = "mid-ranks"))
  expect_equal(object = cont_vs_binary(x = x, y = y, method = 'wilcox', paired = FALSE, verbose = T),
               expected = 0.01854338, tolerance = 1e-7)
  #coin::pvalue(coin::wilcoxsign_test(x[1:10]~x[11:20],distribution = "exact"))
  expect_equal(object = cont_vs_binary(x = x, y = y, method = 'wilcox', paired = TRUE, verbose = T),
               expected = 0.06445312, tolerance = 1e-7)
  #t.test(x~factor(y), paired=F, var.equal = T)$p.value
  expect_equal(object = cont_vs_binary(x = x, y = y, method = 't', paired = FALSE, verbose = T),
               expected = 0.01890551, tolerance = 1e-7)
  #t.test(x~factor(y), paired=T, var.equal = T)$p.value
  #t.test(x[1:10], x[11:20], paired=T, var.Cequal = T)$p.value
  expect_equal(object = cont_vs_binary(x = x, y = y, method = 't', paired = TRUE, verbose = T),
               expected = 0.03394079, tolerance = 1e-7)
  #Testing var.equal = T option in t.test
  #t.test(x~factor(y), paired=F, var.equal = T)$p.value
  expect_equal(object = cont_vs_binary(x = x, y = y, method = 't', paired = FALSE, verbose = T, var.equal = T),
               expected = 0.01875647, tolerance = 1e-7)

  #Testing case where no non-missing pairs
  expect_equal(object = cont_vs_binary(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20))), expected = NA)
  expect_message(object = cont_vs_binary(x = c(rep(1,20),rep(NA,20)), y = c(rep(NA,20),rep(1,20)), verbose = T),
                 regexp = 'There are no observations with non-mising values of both "x" and "y", so p=NA returned')

  #Testing case where all x have same value
  expect_equal(object = cont_vs_binary(x = rep(1,20), y = y), expected = 1)
  expect_message(object = cont_vs_binary(x = rep(1,20), y = y, verbose = T),
                 regexp = '"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')

  #Testing case where all y have same value
  expect_equal(object = cont_vs_binary(x = x, y = rep(1,20)), expected = NA)
  expect_message(object = cont_vs_binary(x = x, y = rep(1,20), verbose = T),
                 regexp = '"y" only has 1 level when considering non-missing values of x, so p=NA returned')
})

test_that("cont_vs_binary throwing internal input checking errors", {
  set.seed(5432322)
  x <- c(rnorm(10,0,3), rnorm(10,3,3))
  y <- c(rep('a', 10), rep('b', 10))
  my_matrix <- matrix(1:10,nrow = 2)

  #Checking x
  expect_error(cont_vs_binary(my_matrix, y = y), 'x must be a vector \\(one-dimensional object\\)')
  expect_error(cont_vs_binary(x = numeric(0), y = y), 'x length must be > 0')
  expect_error(cont_vs_binary(c(NA,NA,NA), y), 'x must have at least a non "NA" value')
  expect_error(cont_vs_binary(letters[1:5],y), 'x must be a numeric vector')

  #Checking y
  expect_error(cont_vs_binary(x, my_matrix), 'y must be a vector \\(one-dimensional object\\)')
  expect_error(cont_vs_binary(x,numeric(0)), 'y length must be > 0')
  expect_error(cont_vs_binary(x,c(NA,NA,NA)), 'y must have at least a non "NA" value')
  expect_error(cont_vs_binary(x,1:10), 'y can not have more than 2 distinct values')
})
