
test_that("geomean is the mean log value, exponentiated",{
  expect_equal(geomean(1:3), exp(sum(log(c(1:3)))/3))
  expect_equal(geomean(exp(1:20)), exp(mean(log(exp(1:20)))))
  expect_equal(geomean(c(1:20, NA)), exp(mean(log(c(1:20, NA)), na.rm = TRUE)))
  expect_equal(geomean(1:10000000), exp(mean(log(c(1:10000000)))))
  expect_equal(geomean(0.1:20), exp(mean(log(0.1:20))))
})

test_that("geomedian is the median log value, exponentiated",{
  expect_equal(unname(geomedian(1:3)), exp(median(log(c(1:3)))))
  expect_equal(unname(geomedian(exp(1:20))), exp(median(log(exp(1:20)))))
  expect_equal(unname(geomedian(c(1:20, NA))), exp(median(log(c(1:20, NA)), na.rm = TRUE)))
  expect_equal(unname(geomedian(1:10000000)), exp(median(log(c(1:10000000)))))
  expect_equal(unname(geomedian(0.1:20)), exp(median(log(0.1:20))))
})

test_that("geosd is the standard deviation of log value, exponentiated",{
  expect_equal(geosd(1:3), exp(sd(log(c(1:3)))))
  expect_equal(geosd(exp(1:20)), exp(sd(log(exp(1:20)))))
  expect_equal(geosd(c(1:20, NA)), exp(sd(log(c(1:20, NA)), na.rm = TRUE)))
  expect_equal(geosd(1:10000000), exp(sd(log(c(1:10000000)))))
  expect_equal(geosd(0.1:20), exp(sd(log(0.1:20))))
})

test_that("geoquantile is the quantile log value, exponentiated",{
  expect_equal(geoquantile(1:3), exp(quantile(log(c(1:3)), type = 2)))
  expect_equal(geoquantile(exp(1:20)), exp(quantile(log(exp(1:20)), type = 2)))
  expect_equal(geoquantile(c(1:20, NA)), exp(quantile(log(1:20), type = 2)))
  expect_equal(geoquantile(1:10000000), exp(quantile(log(1:10000000), type = 2)))
  expect_equal(geoquantile(0.1:20), exp(quantile(log(0.1:20), type = 2)))
  expect_equal(unname(geoquantile(1:100)), exp(fivenum(x = log(1:100))))
})

# x input ----------------------------------------------------------------------
test_that("geo functions error when input length is not long enough",{
  expect_error(geomean(34))
  expect_error(geomean(c(34, 35)))
  expect_error(geosd(34))
  expect_error(geosd(c(34, 35)))
  expect_error(geoquantile(34))
  expect_error(geoquantile(c(34, 35)))
})

test_that("geo functions error when non-numeric vector provided to 'x'",{
  expect_error(geomean(c("green", "blue")), '"x" must be a numeric vector')
  expect_error(geosd(c("green", "blue")), '"x" must be a numeric vector')
  expect_error(geoquantile(c("green", "blue")), '"x" must be a numeric vector')
})

test_that("geo functions error when non-numeric vector provided to 'x'",{
  expect_error(geomean(c(TRUE, TRUE)), '"x" must be a numeric vector')
  expect_error(geosd(c(TRUE, TRUE)), '"x" must be a numeric vector')
  expect_error(geoquantile(c(TRUE, TRUE)), '"x" must be a numeric vector')
})


# Handling of missing values with na.rm
test_that("geo functions return NA if na.rm = FALSE and NA are present in data",{
  expect_identical(geomean(c(NA, 1:5), na.rm = FALSE), as.double(NA))
  expect_identical(geosd(c(NA, 1:5), na.rm = FALSE), as.double(NA))
  expect_error(geoquantile(c(NA, 1:5), na.rm = FALSE), "missing values and NaN's not allowed if 'na.rm' is FALSE")
})

test_that("geo functions error when number used insted of logical",{
  expect_error(geomean(1:5, na.rm = 1))
  expect_error(geosd(1:5, na.rm = 1))
  expect_error(geoquantile(1:5, na.rm = 1))
})

test_that("geo functions error when character used insted of logical",{
  expect_error(geomean(1:5, na.rm = "TRUE"))
  expect_error(geosd(1:5, na.rm = "TRUE"))
  expect_error(geoquantile(1:5, na.rm = "TRUE"))
})

# geoquantile tests-----------------------------------------------------------
test_that("geoquantiles error if provided with probabilites outside [0,1]", {
  expect_error(geoquantile(1:5, probs = 10))
  expect_error(geoquantile(1:5, probs = -1))
  expect_error(geoquantile(1:5, probs = 1:5))
  expect_error(geoquantile(1:5, probs = c("green", "blue")), '"probs" must be numeric')
  expect_error(geoquantile(1:5, probs = TRUE), '"probs" must be numeric')
})

test_that("geoquantiles error if provided with types outside [1,9]", {
  expect_error(geoquantile(1:5, type = 10))
  expect_error(geoquantile(1:5, type = -1))
  expect_error(geoquantile(1:5, type = c("green", "blue")))
  expect_error(geoquantile(1:5, type = TRUE))
})
