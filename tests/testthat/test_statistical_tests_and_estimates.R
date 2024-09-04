context("statistical_tests_and_estimates")


# test round_away_0
test_that("round_away_0 testing various options (no errors)", {
  x = c(0,2.499,2.5,2.5001,3.5,4.05, NA)

  #Note 2.5 goes to 3 as expected
  expect_equal(object = round_away_0(x),
               expected = c(0,2,3,3,4,4,NA))

  #Note 4.05 goes to 4.1 as expected
  expect_equal(object = round_away_0(x, digits = 1),
               expected = c(0,2.5,2.5,2.5,3.5,4.1,NA))
  #Testing for trailing zeros
  expect_equal(object = round_away_0(x, digits = 2, trailing_zeros = TRUE),
               expected = c("0.00", "2.50", "2.50", "2.50", "3.50", "4.05", NA))
  # Allowing all NAs
  expect_equal(object = round_away_0(x = c(NA,NA,NA)),
               expected = c(NA_integer_,NA_integer_,NA_integer_))
})

test_that("round_away_0 throwing errors", {
  set.seed(5432322)
  x <- c(rnorm(10,0,3), rnorm(10,3,3))
  my_matrix <- matrix(1:10,nrow = 2)

  #Checking x
  expect_error(round_away_0(x = my_matrix), '"x" must be a vector \\(one-dimensional object\\)')
  expect_error(round_away_0(x = numeric(0)), '"x" length must be > 0')
  expect_error(round_away_0(x = NULL), '"x" length must be > 0')
  expect_error(round_away_0(x = letters[1:5]), '"x" must be a numeric vector')

  #Checking rounding_digits
  expect_error(round_away_0(x, digits = c(1,2)), '"digits" length must be 1 since expecting scalar')
  expect_error(round_away_0(x, digits = -1), '"digits" must be greater than or equal to 0')
  expect_error(round_away_0(x, digits = 15), '"digits" must be less than or equal to 14')
  expect_error(round_away_0(x, digits = numeric(0)), '"digits" length must be > 0')
  expect_error(round_away_0(x, digits = NA), '"digits" must have at least one non-NA value')
  expect_error(round_away_0(x, digits = 1.5), '"digits" must be whole number\\(s\\)')
})

# test .round_if_numeric
test_that("round_away_0 testing various options (no errors)", {
  x = c(0,2.499,2.5,2.5001,3.5,4.05, NA)

  #Note 2.5 goes to 3 as expected
  expect_equal(object = .round_if_numeric(x),
               expected = c(0,2,3,3,4,4,NA))
  #Note 4.05 goes to 4.1 as expected
  expect_equal(object = .round_if_numeric(x, digits = 1),
               expected = c(0,2.5,2.5,2.5,3.5,4.1,NA))
  #Testing for character
  expect_equal(object = .round_if_numeric(c(NA,letters[1:5])),
               expected = c(NA,letters[1:5]))
  #Testing for trailing zeros
  expect_equal(object = .round_if_numeric(x, digits = 2, trailing_zeros = TRUE),
               expected = c("0.00", "2.50", "2.50", "2.50", "3.50", "4.05", NA))

})




# test two_samp_cont_test
test_that("two_samp_cont_test testing various options (no errors)", {

  ###Testing all four options (wilcox/t and paired/unpaired)###
  set.seed(5432322)
  x <- c(NA,rnorm(10,0,3), rnorm(10,3,3),NA)
  y <- c(rep('a', 10), NA, NA, rep('b', 10))

  # Wilcox Unpaired
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 'wilcox', paired = FALSE, verbose = T),
               expected = as.numeric(coin::pvalue(coin::wilcox_test(x~factor(y),distribution = "exact", ties.method = "mid-ranks")))
               , tolerance = 1e-8)
  # Wilcox Paired
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 'wilcox', paired = TRUE, verbose = T),
               expected = as.numeric(coin::pvalue(coin::wilcoxsign_test(x[1:10]~x[13:22],distribution = "exact")))
               , tolerance = 1e-8)
  # T-Test Unpaired
  #t.test(x[1:10], x[13:22], paired=F, var.equal = F)$p.value
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T),
               expected = t.test(x~factor(y), var.equal = F)$p.value,
               tolerance = 1e-8)
  # T-Test Paired
  expect_equal(object = two_samp_cont_test(x = x[-(11:12)], y = y[-(11:12)], method = 't', paired = TRUE, verbose = T),
               expected = t.test(x[1:10], x[13:22], paired = T, var.equal = F)$p.value,
               tolerance = 1e-8)
  #Testing var.equal = T option in T-Test Unpaired
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T, var.equal = T),
               expected = t.test(x~factor(y), var.equal = T)$p.value,
               tolerance = 1e-8)
  #Testing alternative param can be used
  expect_equal(object = two_samp_cont_test(x = x, y = y, method = 't', paired = FALSE, verbose = T, alternative = 'less'),
               expected = t.test(x~factor(y), alternative = 'less')$p.value,
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





# test two_samp_bin_test
test_that("two_samp_bin_test testing various options (no errors)", {

  ###Testing all four options (wilcox/t and paired/unpaired)###
  set.seed(5432322)
  x <- c(NA, sample(0:1,20,replace = TRUE, prob = c(.65,.25)),
         sample(0:1,20,replace = TRUE, prob = c(.25,.65)), NA)
  y <- c(rep('a', 20), NA, NA, rep('b', 20))

  paired_data_here <- na.omit(
    data.frame(a = x[which(y == levels(factor(y))[1])],
               b = x[which(y == levels(factor(y))[2])])
    )

  # Barnard
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'barnard',
                                          alternative = 'two.sided'),
               expected = Exact::exact.test(table(data.frame(y,x)),
                                            method = 'Z-pooled', to.plot = FALSE,
                                            alternative = 'two.sided')$p.value,
               tolerance = 1e-8)
  # Testing barnard_method
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'barnard',
                                          barnard_method = 'boschloo',
                                          alternative = 'two.sided'),
               expected = Exact::exact.test(table(data.frame(y,x)),
                                            method = 'boschloo', to.plot = FALSE,
                                            alternative = 'two.sided')$p.value,
               tolerance = 1e-8)
     # Testing ... (i.e. npNumbers)
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'barnard',
                                          barnard_method = 'boschloo',
                                          alternative = 'two.sided', npNumbers = 3),
               expected = Exact::exact.test(table(data.frame(y,x)), method = 'boschloo',
                                            to.plot = FALSE, alternative = 'two.sided',
                                            npNumbers = 3)$p.value,
               tolerance = 1e-8)
  # Fisher
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'fisher',
                                          alternative = 'two.sided'),
               expected = fisher.test(x, y, alternative = 'two.sided')$p.value,
               tolerance = 1e-8)
  # Chi-sq
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'chi.sq',
                                          alternative = 'two.sided'),
               expected = chisq.test(x, y)$p.value,
               tolerance = 1e-8)
  # McNemar(paired data)
  expect_equal(object = two_samp_bin_test(x = x[!is.na(y)], y = y[!is.na(y)],
                                          method = 'mcnemar', alternative = 'two.sided'),
               expected = mcnemar.test(paired_data_here$a, paired_data_here$b)$p.value,
               tolerance = 1e-8)

  # Directional, along with factor values
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'barnard',
                                          alternative = 'less'),
               expected = Exact::exact.test(table(data.frame(y,x)), method = 'Z-pooled',
                                            to.plot = FALSE, alternative = 'less')$p.value,
               tolerance = 1e-8)
  expect_equal(object = two_samp_bin_test(x = factor(x), y = y, method = 'barnard',
                                          alternative = 'less'),
               expected = Exact::exact.test(table(data.frame(y,x)), method = 'Z-pooled',
                                            to.plot = FALSE, alternative = 'less')$p.value,
               tolerance = 1e-8)
  expect_equal(object = two_samp_bin_test(x = x, y = y, method = 'barnard',
                                          alternative = 'greater'),
               expected = Exact::exact.test(table(data.frame(y,x)), method = 'Z-pooled',
                                            to.plot = FALSE, alternative = 'greater')$p.value
               , tolerance = 1e-8)
  expect_equal(object = two_samp_bin_test(x = factor(x, levels = 1:0), y = y,
                                          method = 'barnard', alternative = 'less'),
               expected = Exact::exact.test(table(data.frame(y,x)),
                                            method = 'Z-pooled', to.plot = FALSE,
                                            alternative = 'greater')$p.value,
               tolerance = 1e-8)

  # Making sure edge cases work (0 or 100% response rate in a group)
  expect_equal(object = two_samp_bin_test(x = c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0),
                                          y = c(rep('1',5), rep('2',5)),
                                          method = 'barnard', alternative = 'two.sided'),
               expected = Exact::exact.test(table(data.frame(c(rep('1',5), rep('2',5)),
                                                             c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0))),
                                            method = 'Z-pooled', to.plot = FALSE,
                                            alternative = 'two.sided')$p.value,
               tolerance = 1e-8)
  # McNemar(paired data)
  expect_equal(object = two_samp_bin_test(x = c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1),
                                          y = c(rep('1',5), rep('2',5)),
                                          method = 'mcnemar', alternative = 'two.sided'),
               expected = mcnemar.test(c(1, 1, 0, 1, 0),
                                       factor(c( 1, 1, 1, 1, 1), levels = 0:1))$p.value,
               tolerance = 1e-8)

})


test_that("two_samp_bin_test throwing internal .rm_na_and_check checking errors", {
  set.seed(5432322)
  x <- c(NA, sample(0:1,10,replace = TRUE, prob = c(.65,.25)),
         sample(0:1,10,replace = TRUE, prob = c(.25,.65)), NA)
  y <- c(rep('a', 10), NA, NA, rep('b', 10))

  #Testing if x and y are different lengths
  expect_error(two_samp_bin_test(x = rep(0:1,6), y = rep(0:1,5),
                                 method = 'barnard'),
               '"x" and "y" must be the same length')

  #Testing case where no non-missing pairs
  expect_equal(object = two_samp_bin_test(x = c(rep(1,20),rep(NA,20)),
                                          y = c(rep(NA,20),rep(1,20)),
                                          method = 'barnard'),
               expected = NA)
  expect_message(object = two_samp_bin_test(x = c(rep(1,20),rep(NA,20)),
                                            y = c(rep(NA,20),rep(1,20)),
                                            method = 'barnard', verbose = T),
                 regexp = 'There are no observations with non-missing values of both "x" and "y", so p=NA returned')

  #Testing case where all x have same value
  expect_equal(object = two_samp_bin_test(x = rep(1,22), y = y,
                                          method = 'barnard'),
               expected = 1)
  expect_message(object = two_samp_bin_test(x = rep(1,22), y = y,
                                            method = 'barnard', verbose = T),
                 regexp = '"x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned')

  #Testing case where all y have same value
  expect_equal(object = two_samp_bin_test(x = x, y = rep(1,22),
                                          method = 'barnard'),
               expected = NA)
  expect_message(object = two_samp_bin_test(x = x, y = rep(1,22),
                                            method = 'barnard', verbose = T),
                 regexp = '"y" only has 1 level when considering non-missing values of "x", so p=NA returned')
})

# Testing case where p-value equals 1, that machine estimation doesn't add error to values at 1
test_that("two_samp_bin_test doesn't return p-values greater than one", {
  x <- c(rep(1,37),rep(4,2),rep(4,2))
  y <- c(rep(1,21),rep(2,16), rep(1,2),rep(2,2))
  expect_equal(object = two_samp_bin_test(x = x,
                                          y = y,
                                          method = 'fisher',
                                          alternative = 'two.sided'),
               expected = 1,
               tolerance = 0)
})

test_that("two_samp_bin_test throwing internal input checking errors", {
  set.seed(5432322)
  x <- c(sample(0:1,10,replace = TRUE, prob = c(.65,.25)),
         sample(0:1,10,replace = TRUE, prob = c(.25,.65)))
  y <- c(rep('a', 10), rep('b', 10))
  my_matrix <- matrix(1:10, nrow = 2)

  #Checking x
  expect_error(two_samp_bin_test(my_matrix, y = y, method = 'barnard'),
               '"x" must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_bin_test(x = numeric(0), y = y, method = 'barnard'),
               '"x" length must be > 0')
  expect_error(two_samp_bin_test(c(NA,NA,NA), y, method = 'barnard'),
               '"x" must have at least one non-NA value')
  expect_error(two_samp_bin_test(letters[1:5],y, method = 'barnard'),
               '"x" cannot have more than 2 distinct values')

  #Checking y
  expect_error(two_samp_bin_test(x, my_matrix, method = 'barnard'),
               '"y" must be a vector \\(one-dimensional object\\)')
  expect_error(two_samp_bin_test(x,numeric(0), method = 'barnard'),
               '"y" length must be > 0')
  expect_error(two_samp_bin_test(x,c(NA,NA,NA), method = 'barnard'),
               '"y" must have at least one non-NA value')
  expect_error(two_samp_bin_test(x,1:10, method = 'barnard'),
               '"y" cannot have more than 2 distinct values')

  #Testing paired errors
  expect_error(two_samp_bin_test(x = c(NA,rep(0:1,4),0),
                                 y = c(rep(0:1,4),0,NA), method = 'mcnemar'),
               'When "paired" = TRUE "y" cannot have missing values')
  expect_error(two_samp_bin_test(x = c(NA,rep(0:1,4),0),
                                 y = c(rep(0:1,5),1), method = 'mcnemar'),
               'When "paired" = TRUE "y" must have the same number of samples for each level')
})







# test cor_test
test_that("cor_test testing various options (no errors)", {
  ###Testing three methods
  set.seed(47813458)
  x <- c(NA,rnorm(5,0,3), rnorm(5,3,3),NA)
  y <- c(rnorm(5,0,1),NA, rnorm(5,2,1),NA)

  # pearson
  expect_identical(object = cor_test(x = x, y = y, method = 'pearson'),
               expected = as.double(cor.test(x,
                                             y,
                                             method = 'pearson')$p.value)
  )
  # kendall
  expect_identical(object = cor_test(x = x, y = y, method = 'kendall'),
               expected = as.double(cor.test(x,
                                             y,
                                             method = 'kendall')$p.value)
  )
  # spearman no ties
  expect_identical(object = cor_test(x = x, y = y, method = 'spearman'),
               expected = as.double(cor.test(x,
                                             y,
                                             method = 'spearman')$p.value)
  )
  # spearman ties
  set.seed(4312)
  tmp_expected <- as.double(coin::pvalue(
    coin::spearman_test(x~y,
                        data = data.frame(x = c(x,x), y = c(y,y)),
                        distribution = coin::approximate(10000)
    )))
  expect_identical(object = cor_test(x = c(x,x), y = c(y,y),
                                 method = 'spearman', seed = 4312,
                                 nresample = 10000),
               expected = tmp_expected
  )
  expect_message(object = cor_test(x = c(x,x), y = c(y,y),
                                 method = 'spearman', verbose = TRUE),
               regexp = 'Either "x" or "y" has ties, so using approximate method.'
  )
  # spearman ties but not exact
  expect_identical(object = cor_test(x = c(x,x), y = c(y,y),
                                 method = 'spearman', exact = FALSE),
               expected = as.double(cor.test(c(x,x),
                                             c(y,y),
                                             method = 'spearman',
                                             exact = FALSE)$p.value)
  )

  #confirming seed is restored
  old_seed <- get(".Random.seed", globalenv(), mode = "integer",
                  inherits = FALSE)
  xx <- cor_test(x = c(x,x), y = c(y,y),
                 method = 'spearman', seed = 47861684, nresample = 10000)
  expect_identical(object = get(".Random.seed", globalenv(), mode = "integer",
                                inherits = FALSE),
                   expected = old_seed
  )

})


test_that("cor_test throwing internal .rm_na_and_check checking errors", {
  set.seed(47813458)
  x <- c(NA,rnorm(5,0,3), rnorm(5,3,3),NA)
  y <- c(rnorm(5,0,1),NA, rnorm(5,2,1),NA)

  #Testing if x and y are different lengths
  expect_error(cor_test(x = 1:9, y = 1:10), '"x" and "y" must be the same length')

  #Testing case where no non-missing pairs
  expect_identical(object = cor_test(x = c(1:20,rep(NA,20)), y = c(rep(NA,20),1:20)), expected = NA)
  expect_message(object = cor_test(x = c(1:20,rep(NA,20)), y = c(rep(NA,20),1:20), verbose = T),
                 regexp = 'There are <2 observations with non-missing values of both "x" and "y", so p=NA returned')

  #Testing case where all x have same value
  expect_identical(object = cor_test(x = rep(1,12), y = y), expected = NA)
  expect_message(object = cor_test(x = rep(1,12), y = y, verbose = T),
                 regexp = 'There are <2 observations with non-missing values of both "x" and "y", so p=NA returned')

  #Testing case where all y have same value
  expect_identical(object = cor_test(x = x, y = rep(1,12)), expected = NA)
  expect_message(object = cor_test(x = x, y = rep(1,12), verbose = T),
                 regexp = 'There are <2 observations with non-missing values of both "x" and "y", so p=NA returned')
})

test_that("cor_test throwing internal input checking errors", {
  set.seed(47813458)
  x <- c(NA,rnorm(5,0,3), rnorm(5,3,3),NA)
  y <- c(rnorm(5,0,1),NA, rnorm(5,2,1),NA)
  my_matrix <- matrix(1:10,nrow = 2)

  #Checking x
  expect_error(cor_test(my_matrix, y = y),
               regexp = '"x" must be a vector \\(one-dimensional object\\)')
  expect_error(cor_test(x = numeric(0), y = y),
               regexp = '"x" length must be > 0')
  expect_error(cor_test(c(NA,NA,NA), y),
               regexp = '"x" must have at least one non-NA value')
  expect_error(cor_test(letters[1:5],y),
               regexp = '"x" must be a numeric vector')

  #Checking y
  expect_error(cor_test(x, my_matrix),
               regexp = '"y" must be a vector \\(one-dimensional object\\)')
  expect_error(cor_test(x,numeric(0)),
               regexp = '"y" length must be > 0')
  expect_error(cor_test(x,c(NA,NA,NA)),
               regexp = '"y" must have at least one non-NA value')
  expect_error(cor_test(x,letters[1:5]),
               regexp = '"y" must be a numeric vector')
})


test_that("test-wilson_ci", {

  # check x
  expect_error(wilson_ci(c(NA, NA, NA)), '"x" must have at least one non-NA value')
  expect_error(wilson_ci(c()), '"x" length must be > 0')
  expect_error(wilson_ci(c(NA, NA, NA)), '"x" must have at least one non-NA value')
  expect_error(wilson_ci(x = c("F", "T", "F", "T")),
               '"x" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
  expect_error(wilson_ci(x = factor(c("F", "T", "F", "T"))),
               '"x" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')

  # wilson_ci() should match binom::binom.wilson()
  x <- c(1, 1, 1, 0, 0)
  expect_equal(wilson_ci(x), binom::binom.wilson(3, 5)[, c('mean', 'lower', 'upper')])
  expect_equal(wilson_ci(as.logical(x)), binom::binom.wilson(3, 5)[, c('mean', 'lower', 'upper')])
  # testing edge cases
  expect_equal(wilson_ci(rep(0, 50)), binom::binom.wilson(0, 50)[, c('mean', 'lower', 'upper')])
  expect_equal(wilson_ci(rep(1, 50)), binom::binom.wilson(50, 50)[, c('mean', 'lower', 'upper')])

  # check conf.level
  x <- c(rep(0, 200), rep(1, 400))
  expect_error(wilson_ci(x, conf.level = 1), '"conf.level" must be less than or equal to 0.999999999999')
  expect_error(wilson_ci(x, conf.level = -.95), '"conf.level" must be greater than or equal to 0')
  expect_error(wilson_ci(x, conf.level = 2), '"conf.level" must be less than or equal to 0.999999999999')
  expect_error(wilson_ci(x, conf.level = ".95"), '"conf.level" must be a numeric vector')
  expect_error(wilson_ci(x, conf.level = TRUE), '"conf.level" must be a numeric vector')

})



test_that("test-binom_ci", {

  # check x
  expect_error(binom_ci(c(NA, NA, NA)), '"x" must have at least one non-NA value')
  expect_error(binom_ci(c()), '"x" length must be > 0')
  expect_error(binom_ci(c(NA, NA, NA)), '"x" must have at least one non-NA value')
  expect_error(binom_ci(x = c("F", "T", "F", "T")),
               '"x" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
  expect_error(binom_ci(x = factor(c("F", "T", "F", "T"))),
               '"x" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')

  # binom_ci() should match binom::binom.confint()
  x <- c(1, 1, 1, 0, 0)
  expect_equal(binom_ci(x), binom::binom.wilson(3, 5))
  expect_equal(binom_ci(as.logical(x)), binom::binom.wilson(3, 5))
  expect_equal(binom_ci(x, methods = 'all'),
               binom::binom.confint(3, 5, methods = 'all')) %>% suppressWarnings
  # testing edge cases
  expect_equal(binom_ci(rep(0, 50)), binom::binom.wilson(0, 50))
  expect_equal(binom_ci(rep(1, 50)), binom::binom.wilson(50, 50))

  # check conf.level
  x <- c(rep(0, 200), rep(1, 400))
  expect_error(binom_ci(x, conf.level = 1), '"conf.level" must be less than or equal to 0.999999999999')
  expect_error(binom_ci(x, conf.level = -.95), '"conf.level" must be greater than or equal to 0')
  expect_error(binom_ci(x, conf.level = 2), '"conf.level" must be less than or equal to 0.999999999999')
  expect_error(binom_ci(x, conf.level = ".95"), '"conf.level" must be a numeric vector')
  expect_error(binom_ci(x, conf.level = TRUE), '"conf.level" must be a numeric vector')

})








