context("test-escape")

test_that("escape will put slashes to escape control characters", {
  example = c("testvalue", "test_value", "ampersand&")
  escaped_example1 <- escape(example)
  escaped_example2 <- escape("String_Entry %")

  testthat::expect_equal(escaped_example1, c("testvalue", "test\\_value", "ampersand\\&"))
  testthat::expect_equal(escaped_example2, "String\\_Entry \\%")
})
