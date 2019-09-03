context("test-group_row_collapse")

test_that("Does not allow non data.frames", {
  vect<-c(1,2,3,4)
  expect_error(group_row_collapse(vect))
})


test_that("All column entries must be found in the data.frame", {
  sample_df<-data.frame(
    x=c(1,1,1,2,2),
    y=c(2,3,3,2,3),
    z=c(1,2,3,4,5))
  expect_error(group_row_collapse(vect,x,y,z,a))
})


test_that("Output columns are reordered and duplicated rows (based on columns pass) are removed", {

  # z output order is 1:5
  sample_df<-data.frame(
    x=c(2,2,1,1,1),
    y=c(2,3,2,3,3),
    z=c(4,5,1,2,3))

  collapsed<-group_row_collapse(sample_df,x,y)

  expect_equal(collapsed$z,1:5)
  expect_equivalent(collapsed,
                    data.frame(x=c("1","","","2",""),
                               y=c("2","3","","2","3"),
                               z=c(1,2,3,4,5),
                               stringsAsFactors = FALSE))
})


test_that("Handle spaces in column names of the input df", {

  # z output order is 1:5
  sample_df<-data.frame(
    `x 123`=c(2,2,1,1,1),
    y=c(2,3,2,3,3),
    z=c(4,5,1,2,3),
    check.names = FALSE)

  collapsed<-group_row_collapse(sample_df,`x 123`,y)

  expect_equal(collapsed$z,1:5)
  expect_equivalent(collapsed,
                    data.frame(`x 123`=c("1","","","2",""),
                               y=c("2","3","","2","3"),
                               z=c(1,2,3,4,5),
                               stringsAsFactors = FALSE,check.names = FALSE))
})
