context("trapz_sorted")

test_that("trapz_sorted performs similarly to trapz", {
  library(pracma)
  set.seed(93)
  n <- 101
  # Area under the sin curve
  x <- seq(0, pi, len = n)
  y <- sin(x)
  expect_equal(trapz_sorted(x,y), trapz(x,y))
  # Area under a monotonic increasing curve
  y <- runif(n, 0, 42)
  x <- seq(along = y)
  expect_equal(trapz_sorted(x,y), trapz(x,y))
  # Area under a curve with negative y values
  y <- runif(n, -42, 42)
  x <- seq(along = y)
  expect_equal(trapz_sorted(x,y), trapz(x,y))
  # Area under a curve with negative x values
  y <- runif(n, 0, 42)
  x <- seq(along = y) * -1
  x <- x[order(x)]
  expect_equal(trapz_sorted(x,y), trapz(x,y))
  # Area under a curve with negative x and y values
  y <- runif(n, -42, 42)
  expect_equal(trapz_sorted(x,y), trapz(x,y))
})

test_that("trapz_sorted is not the same trapz", {
  library(pracma)
  set.seed(93)
  n <- 101
  # Area under a monotonic declining curve
  y <- runif(n, 0, 42)
  x <- seq(along = y) * -1
  expect_gt(trapz_sorted(x,y), trapz(x,y))
  # Area under an unsorted curve
  x <- runif(n, 0, 42)
  expect_gt(trapz_sorted(x,y), trapz(x,y))

})
