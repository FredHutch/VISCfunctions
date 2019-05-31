context("reproducibility_tables")


test_that("get_full_name() testing", {

  # Garbage username should just return username
  expect_equal(object = get_full_name('fakeuser'), expected = 'fakeuser')


})


test_that("get_session_info() testing", {

  # Specifics change depending on user specifics and environment/packages loading, but can check some details
  temp_session_info <- get_session_info()

  # testing dimension
  expect_equal(object = dim(temp_session_info$platform_table), expected = c(13,2))

  dim_expected <- ifelse(any(colnames(temp_session_info$packages_table) == "data.version"), 5, 4)
  expect_equal(object = ncol(temp_session_info$packages_table), expected = dim_expected)

})

