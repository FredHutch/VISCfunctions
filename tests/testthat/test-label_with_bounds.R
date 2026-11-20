test_that("formatting composes with independent bounds", {
  fmt <- function(x) paste0(100 * x, "%")
  x <- c(1, 0, 0.5)
  expect_identical(label_with_bounds(fmt, TRUE, TRUE)(x),
                   c("\u2265 100%", "\u2264 0%", "50%"))
  expect_identical(label_with_bounds(fmt, FALSE, FALSE)(x), fmt(x))
  expect_identical(label_with_bounds(fmt, TRUE, FALSE)(x),
                   c("100%", "\u2264 0%", "50%"))
  expect_identical(label_with_bounds(fmt)(x),
                   c("\u2265 100%", "0%", "50%"))
})

test_that("edge cases preserve numeric boundary semantics", {
  lab <- label_with_bounds(lower = TRUE, upper = TRUE)
  expect_identical(lab(c(2, 1, 2, NA, Inf)),
                   c("\u2265 2", "\u2264 1", "\u2265 2", NA_character_, "Inf"))
  expect_identical(lab(numeric()), character())
  x <- c(NA_real_, NaN, -Inf, Inf)
  expect_identical(lab(x), as.character(x))
  expect_error(lab(c(1, 1)), "one distinct finite break")
  expect_identical(label_with_bounds()(1), "\u2265 1")
  expect_identical(label_with_bounds(function(x) rep(NA_character_, length(x)))(1:2),
                   rep(NA_character_, 2))
})

test_that("invalid arguments and formatter results are rejected", {
  expect_error(label_with_bounds(1), "must be a function")
  for (flag in list(NA, 1, logical(), c(TRUE, FALSE))) {
    expect_error(label_with_bounds(lower = flag), "TRUE or FALSE")
    expect_error(label_with_bounds(upper = flag), "TRUE or FALSE")
  }
  expect_error(label_with_bounds()("1"), "numeric vector")
  expect_error(label_with_bounds()(matrix(1:4, 2)), "numeric vector")
  expect_error(label_with_bounds(identity)(1:2), "character label")
  expect_error(label_with_bounds(function(x) "one")(1:2), "character label")
})

test_that("scales percentage formatters compose with bounds", {
  skip_if_not_installed("scales")
  x <- c(0, 0.5, 1)
  expected <- c("\u2264 0%", "50%", "\u2265 100%")
  expect_identical(label_with_bounds(scales::label_percent(accuracy = 1),
                                     TRUE, TRUE)(x), expected)
  expect_identical(label_with_bounds(scales::percent, TRUE, TRUE)(x), expected)
})
