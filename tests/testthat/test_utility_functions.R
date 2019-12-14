context("utility_functions")

test_that("retrieve user name on a variety of platforms", {
  # Windows
  cur_user_name <- Sys.getenv("USERNAME")
  on.exit(expr = Sys.setenv(USERNAME = cur_user_name))
  Sys.setenv(USERNAME = "eachung")
  uname <- getUsername()
  expect_identical("Eva Chung", uname)

  # TODO: Linux, Darwin
})
