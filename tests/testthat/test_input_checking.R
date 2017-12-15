context("input_checking")

# test .check_numeric_input
test_that(".check_numeric_input error-free when expected", {
  expect_error(.check_numeric_input(1:10), NA)
  expect_error(.check_numeric_input(c(1:10,NA,1:20,NA)), NA)
  my_vals <- c(1,5,2,NA)
  expect_error(.check_numeric_input(my_vals), NA)
})

test_that(".check_numeric_input throws appropriate errors", {
  expect_error(.check_numeric_input(numeric(0)), 'numeric\\(0\\) length must be > 0')

  expect_error(.check_numeric_input(c(NA,NA,NA)), 'c\\(NA, NA, NA\\) must have non "NA" values')

  expect_error(.check_numeric_input(letters[1:5]), 'letters\\[1:5\\] must be a numeric vector')
  my_letters <- letters[1:10]
  expect_error(.check_numeric_input(my_letters), 'my_letters must be a numeric vector')

  expect_error(.check_numeric_input(1:10, lower_bound = 3), '1\\:10 must be greater than 3')

  expect_error(.check_numeric_input(1:10, lower_bound = 0, upper_bound = 5), '1\\:10 must be less than 5')
})


# test .check_binary_input
test_that(".check_binary_input error-free when expected", {
  expect_error(.check_binary_input(c('a','b')), NA)
  expect_error(.check_binary_input(1:2), NA)
  expect_error(.check_binary_input(5), NA)
  expect_error(.check_binary_input(factor(rep(1:2,10))), NA)
  my_vals <- c(1,10,1,1,NA,10)
  expect_error(.check_binary_input(my_vals), NA)
})

test_that(".check_binary_input throws appropriate errors", {
  expect_error(.check_binary_input(numeric(0)), 'numeric\\(0\\) length must be > 0')

  expect_error(.check_binary_input(c(NA,NA,NA)), 'c\\(NA, NA, NA\\) must have non "NA" values')

  expect_error(.check_binary_input(1:10), '1\\:10 can not have more than 2 distinct values')
})


# test .check_response_input
test_that(".check_response_input error-free when expected", {
  expect_error(.check_response_input(c(0,0,0,1,1,0,0,1)), NA)
  expect_error(.check_response_input(c(T,T,F,F,T,F)), NA)
  my_vals <- c(1,0,1,1,NA,0)
  expect_error(.check_response_input(my_vals), NA)
})

test_that(".check_response_input throws appropriate errors", {
  expect_error(.check_response_input(numeric(0)), 'numeric\\(0\\) length must be > 0')

  expect_error(.check_response_input(c(NA,NA,NA)), 'c\\(NA, NA, NA\\) must have non "NA" values')

  expect_error(.check_response_input(1:10), '1\\:10 must contain only 0/1 or T/F values')
  my_letters <- c('a','b')
  expect_error(.check_response_input(my_letters), 'my_letters must contain only 0/1 or T/F values')
})





