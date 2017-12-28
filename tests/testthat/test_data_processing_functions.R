context("data_processing_functions")

# test .rm_na_and_check
test_that(".rm_na_and_check returning data.frame", {
  x <- c(NA,NA,1:10,NA,100)
  y_bin <- c(c(NA,2,rep(1:2,6)))
  y_cont <- c(NA,2,1:10,NA,100)

  #Note first row has NA in both variable so expecting that row to be dropped
  expect_equal(.rm_na_and_check(x, y_bin),
               expected = data.frame(x = x,y = y_bin)[-1,])
  #Note first and 13th row has NA in both variable so expecting that row to be dropped
  expect_equal(.rm_na_and_check(x, y_cont, y_type = 'continuous'),
               expected = data.frame(x = x,y = y_cont)[-c(1,13),])
})
test_that(".rm_na_and_check returning p value", {
  #no complete cases
  expect_equal(.rm_na_and_check(c(1,1,NA,NA), c(NA,NA,1,1)), expected = NA)
  expect_message(.rm_na_and_check(c(1,1,NA,NA), c(NA,NA,1,1), verbose = T),
                 regexp = 'There are no observations with non-mising values of both "x" and "y", so p=NA returned')

  expect_equal(.rm_na_and_check(c(1,1,1,1), c(NA,NA,NA,NA)), expected = NA)
  expect_message(.rm_na_and_check(c(1,1,NA,NA), c(NA,NA,1,1), verbose = T),
                 regexp = 'There are no observations with non-mising values of both "x" and "y", so p=NA returned')

  #x only one value
  expect_equal(.rm_na_and_check(rep(1,6), rep(1:2,3)), expected = 1)
  expect_message(.rm_na_and_check(rep(1,6), rep(1:2,3), verbose = T),
                 regexp = '"x" only has 1 distinct value when considering non-missing values of y, so p=1 returned')
  #binary y only one value
  expect_equal(.rm_na_and_check(rep(1:2,3),rep(1,6)), expected = NA)
  expect_message(.rm_na_and_check(rep(1:2,3),rep(1,6), verbose = T),
                 regexp = '"y" only has 1 level when considering non-missing values of x, so p=NA returned')

  #continuous y only one value
  expect_equal(.rm_na_and_check(rep(1:2,3),rep(1,6), y_type = 'cont'), expected = 1)
  expect_message(.rm_na_and_check(rep(1:2,3),rep(1,6), y_type = 'cont', verbose = T),
                 regexp = '"y" only has 1 distinct value when considering non-missing values of x, so p=1 returned')

})