context("trapz_sorted")

test_that("trapz_sorted performs similarly to trapz", {
  set.seed(93)
  n <- 101
  # Area under the sin curve
  x <- seq(0, pi, len = n)
  y <- sin(x)
  expect_equal(trapz_sorted(x,y), pracma::trapz(x,y))
  # Area under a monotonic increasing curve
  y <- runif(n, 0, 42)
  x <- seq(along = y)
  expect_equal(trapz_sorted(x,y), pracma::trapz(x,y))
  # Area under a curve with negative y values
  y <- runif(n, -42, 42)
  x <- seq(along = y)
  expect_equal(trapz_sorted(x,y), pracma::trapz(x,y))
  # Area under a curve with negative x values
  y <- runif(n, 0, 42)
  x <- seq(along = y) * -1
  x <- x[order(x)]
  expect_equal(trapz_sorted(x,y), pracma::trapz(x,y))
  # Area under a curve with negative x and y values
  y <- runif(n, -42, 42)
  expect_equal(trapz_sorted(x,y), pracma::trapz(x,y))
})

test_that("trapz_sorted is not the same trapz", {
  set.seed(93)
  n <- 101
  # Area under a monotonic curve with declining x values
  y <- runif(n, 0, 42)
  x <- seq(along = y) * -1
  expect_gt(trapz_sorted(x,y), pracma::trapz(x,y))
  # Area under an unsorted curve
  x <- runif(n, 0, 42)
  expect_gt(trapz_sorted(x,y), pracma::trapz(x,y))
  # There are NA values in the data
  x <- seq(0, pi, len = n)
  y <- sin(x)
  y[23] <- NA
  expect_gte(trapz_sorted(x,y), 1)
})

test_that("trapz_sorted produces errors or warnings when there are problems with input data", {
  set.seed(93)
  # Logicals supplied instead of numbers
  x <- 1:10
  y <- as.logical(rbinom(10, 1, 0.5))
  expect_error(trapz_sorted(x,y),
                 '"y" must be a numeric vector')
  # Matrix supplied instead of vector
  x <- 1:100
  y <- matrix(1:100, 10)
  expect_error(trapz_sorted(x,y),
               '"y" must be a vector \\(one-dimensional object\\)')
  # x and y are different lengths
  y <- 1:10
  expect_error(trapz_sorted(x,y),
               "Arguments 'x', 'y' must be vectors of the same length.")
  # Less than two non-missing values
  x <- 1:2
  y <- c(runif(1, 0, 42), NA)
  expect_warning(trapz_sorted(x,y),
                 "There are less than 2 observations with non-missing values of both 'x' and 'y', so NA returned.")
  # na.rm is false and there are NA values in the y vector
  x <- 1:10
  y <- runif(10, 0, 42)
  y[7] <- NA
  expect_warning(trapz_sorted(x,y, na.rm = FALSE),
                 "NA values are present in the 'y' values. Returns NA.")
  # na.rm is false and there are NA values in the x vector
  y <- runif(10, 0, 42)
  x[7] <- NA
  expect_warning(trapz_sorted(x,y, na.rm = FALSE),
                 "NA values are present in the 'x' values. Returns NA.")
})
