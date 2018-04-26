context("input_checking")

# test .check_numeric_input
test_that(".check_numeric_input error-free when expected", {
  expect_null(.check_numeric_input(1:10))
  expect_null(.check_numeric_input(c(1:10,NA,1:20,NA)))
  my_vals <- c(1,5,2,NA)
  expect_null(.check_numeric_input(my_vals))
  expect_null(.check_numeric_input(1, scalar = TRUE))
  expect_null(.check_numeric_input(-10:10, whole_num = TRUE))

})

test_that(".check_numeric_input throws appropriate errors", {
  my_matrix <- matrix(1:10,nrow = 2)
  expect_error(.check_numeric_input(my_matrix), '"my_matrix" must be a vector \\(one-dimensional object\\)')

  expect_error(.check_numeric_input(numeric(0)), '"numeric\\(0\\)" length must be > 0')

  expect_error(.check_numeric_input(c(NA,NA,NA)), '"c\\(NA, NA, NA\\)" must have at least one non-NA value')

  expect_error(.check_numeric_input(letters[1:5]), '"letters\\[1:5\\]" must be a numeric vector')
  my_letters <- letters[1:10]
  expect_error(.check_numeric_input(my_letters), '"my_letters" must be a numeric vector')

  expect_error(.check_numeric_input(1:10, lower_bound = 3), '"1\\:10" must be greater than or equal to 3')

  expect_error(.check_numeric_input(1:10, lower_bound = 0, upper_bound = 5), '"1\\:10" must be less than or equal to 5')

  expect_error(.check_numeric_input(1:10, scalar = TRUE), '"1\\:10" length must be 1 since expecting scalar')

  expect_error(.check_numeric_input(1.5:10.5, whole_num = TRUE), '"1.5\\:10.5" must be whole number\\(s\\)')

})


# test .check_binary_input
test_that(".check_binary_input error-free when expected", {
  expect_null(.check_binary_input(c('a','b')))
  expect_null(.check_binary_input(1:2, paired = TRUE))
  expect_null(.check_binary_input(5))
  expect_null(.check_binary_input(factor(rep(1:2,10))))
  expect_null(.check_binary_input(c(F,T,T,F)))
  my_vals <- c(1,10,1,1,NA,10)
  expect_null(.check_binary_input(my_vals))
})

test_that(".check_binary_input throws appropriate errors", {
  my_matrix <- matrix(rep(c('a','b'),5),nrow = 2)
  expect_error(.check_binary_input(my_matrix), '"my_matrix" must be a vector \\(one-dimensional object\\)')
  expect_error(.check_binary_input(c(1:2,NA), paired = TRUE), 'When "paired" = TRUE "c\\(1:2, NA\\)" cannot have missing values')
  expect_error(.check_binary_input(numeric(0)), '"numeric\\(0\\)" length must be > 0')
  expect_error(.check_binary_input(c(NA,NA,NA)), '"c\\(NA, NA, NA\\)" must have at least one non-NA value')
  expect_error(.check_binary_input(1:10), '"1:10" cannot have more than 2 distinct values')
  expect_error(.check_binary_input(1, paired = TRUE), 'When "paired" = TRUE "1" must have exactly 2 distinct values')
  expect_error(.check_binary_input(c(1,1,2), paired = TRUE), 'When "paired" = TRUE "c\\(1, 1, 2\\)" must have the same number of samples for each level')
})


# test .check_response_input
test_that(".check_response_input error-free when expected", {
  expect_null(.check_response_input(c(0,0,0,1,1,0,0,1)))
  expect_null(.check_response_input(c(T,T,F,F,T,F)))
  my_vals <- c(1,0,1,1,NA,0)
  expect_null(.check_response_input(my_vals))
})

test_that(".check_response_input throws appropriate errors", {
  my_matrix <- matrix(rep(0:1,5),nrow = 2)
  expect_error(.check_response_input(my_matrix), '"my_matrix" must be a vector \\(one-dimensional object\\)')

  expect_error(.check_response_input(numeric(0)), '"numeric\\(0\\)" length must be > 0')

  expect_error(.check_response_input(c(NA,NA,NA)), '"c\\(NA, NA, NA\\)" must have at least one non-NA value')

  expect_error(.check_response_input(1:10), '"1:10" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
  my_letters <- c('a','b')
  expect_error(.check_response_input(my_letters), '"my_letters" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
  my_letters <- c('T','F')
  expect_error(.check_response_input(my_letters), '"my_letters" must be a numeric vector containing only 0/1 values or a logical vector containing only T/F values')
})





