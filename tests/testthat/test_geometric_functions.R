


test_that("geomean is the mean log value, exponentiated",{
  expect_equal(geomean(1:2), exp(sum(log(c(1:2)))/2))
  expect_equal(geomean(c(-1:100, NA), threshold = 1), exp(mean(log(c(1, 1, 1:100, NA)), na.rm = TRUE)))
  expect_equal(geomean(exp(1:20)), exp(mean(log(exp(1:20)))))
  expect_equal(geomean(1:10000000), exp(mean(log(c(1:10000000)))))
  expect_equal(geomean(0.1:20, threshold = NULL), exp(mean(log(0.1:20))))
})

test_that("geomedian is the median log value, exponentiated",{
  expect_equal(unname(geomedian(1:2)), exp(median(log(c(1:2)))))
  expect_equal(unname(geomedian(0:2)), exp(median(log(c(0:2)))))
  expect_equal(unname(geomedian(c(0:100, NA))), exp(median(log(c(0:100, NA)), na.rm = TRUE)))
  expect_equal(unname(geomedian(exp(1:20))), exp(median(log(exp(1:20)))))
  expect_equal(unname(geomedian(1:10000000)), exp(median(log(c(1:10000000)))))
  expect_equal(unname(geomedian(0.1:20, threshold = NULL)),
                      exp(median(log(0.1:20))))
})

test_that("geosd is the standard deviation of log value, exponentiated",{
  expect_equal(geosd(1:2), exp(sd(log(c(1:2)))))
  expect_equal(geosd(c(1:100, NA)), exp(sd(log(c(1:100, NA)), na.rm = TRUE)))
  expect_equal(geosd(exp(1:20)), exp(sd(log(exp(1:20)))))
  expect_equal(geosd(1:10000000), exp(sd(log(c(1:10000000)))))
  expect_equal(geosd(0.1:20, threshold = NULL), exp(sd(log(0.1:20))))
})

test_that("geoquantile is the quantile log value, exponentiated",{
  expect_equal(geoquantile(1:2), exp(quantile(log(c(1:2)), type = 2)))
  expect_equal(geoquantile(0:2), exp(quantile(log(c(1,1:2)), type = 2)))
  expect_equal(geoquantile(c(0:100, NA)), exp(quantile(log(c(1,1:100, NA)), na.rm =TRUE)))
  expect_equal(geoquantile(exp(1:20)), exp(quantile(log(exp(1:20)), type = 2)))
  expect_equal(geoquantile(1:10000000), exp(quantile(log(1:10000000), type = 2)))
  expect_equal(geoquantile(0.1:20, threshold = NULL),
               exp(quantile(log(0.1:20), type = 2)))
  expect_equal(unname(geoquantile(1:100)), exp(fivenum(x=log(1:100))))
})

# x input ----------------------------------------------------------------------
test_that("geo functions error when scalar data is provided to 'x'",{
  expect_error(geomean(34), '"x" must have a length more than two')
  expect_error(geosd(34), '"x" must have a length more than two')
  expect_error(geoquantile(34), '"x" must have a length more than two')
})

test_that("geo functions remove zero values (not done in aritmetic functions) and replace with 1.",{
  expect_equal(geomean(0:2, threshold = 1), exp(mean(log(c(1, 1:2, NA)), na.rm = TRUE)))
  expect_equal(unname(geomedian(0:2, threshold = 1)), exp(median(log(c(1, 1:2)))))
  expect_equal(geosd(0:2, threshold = 1), exp(sd(log(c(1, 1:2)))))
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


# na.rm----------------------------------------------------------------------
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
# threshold tests----------------------------------------------------------------
test_that("geo functions error when threshold is less than 0, or non-numerical",{
  expect_equal(round(geomean(1:5, threshold = 0),5), round(2.605171, 5))
  expect_error(geomean(-1:5, threshold = 0), '"threshold" must be a positive numeral greater than zero.')
  expect_error(geosd(-1:5, threshold = 0), '"threshold" must be a positive numeral greater than zero.')
  expect_error(geoquantile(-1:5, threshold = 0), '"threshold" must be a positive numeral greater than zero.')
})

test_that("geo functions error when threshold is longer than 1",{
  expect_error(geomean(1:5, threshold = 1:5), '"threshold" must have a length of one.')
  expect_error(geosd(1:5, threshold = 1:5), '"threshold" must have a length of one.')
  expect_error(geoquantile(1:5, threshold = 1:5), '"threshold" must have a length of one.')
})

test_that("geo functions error when threshold is null",{
  expect_error(geomean(-1:5, threshold = NULL, verbose = FALSE),
               '"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  expect_error(geosd(-1:5, threshold = NULL, verbose = FALSE),
               '"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  expect_error(geoquantile(-1:5, threshold = NULL, verbose = FALSE),
               '"threshold" must be a positive numeral greater than zero when there are values in the data are at or less than zero')
  expect_warning(geomean(0.1:5, threshold = NULL, verbose = TRUE))
  expect_warning(geosd(0.1:5, threshold = NULL, verbose = TRUE))
  expect_warning(geoquantile(0.1:5, threshold = NULL, verbose = TRUE))
})

test_that("geo functions error when threshold is higher than the data.",{
  expect_error(geomean(1:5, threshold = 6), '"threshold" must be less than at least one value of "x"')
  expect_error(geosd(1:5, threshold = 6), '"threshold" must be less than at least one value of "x"')
  expect_error(geoquantile(1:5, threshold = 6), '"threshold" must be less than at least one value of "x"')
})

test_that("geo functions error when threshold is less than 0, or non-numerical",{
  expect_error(geomean(-1:5, threshold = TRUE), '"threshold" must be numeric or null')
  expect_error(geomean(-1:5, threshold = TRUE), '"threshold" must be numeric or null')
  expect_error(geosd(-1:5, threshold = TRUE), '"threshold" must be numeric or null')
  expect_error(geoquantile(-1:5, threshold = TRUE), '"threshold" must be numeric or null')
  expect_error(geomean(-1:5, threshold = "TRUE"), '"threshold" must be numeric or null')
  expect_error(geomean(-1:5, threshold = "TRUE"), '"threshold" must be numeric or null')
  expect_error(geosd(-1:5, threshold = "TRUE"), '"threshold" must be numeric or null')
  expect_error(geoquantile(-1:5, threshold = "TRUE"), '"threshold" must be numeric or null')
})

# geoquantile tests-----------------------------------------------------------
test_that("geoquantiles error if provided with probabilites outside [0,1]", {
  expect_error(geoquantile(1:5, probs = 10))
  expect_error(geoquantile(1:5, probs = -1))
  expect_error(geoquantile(1:5, probs = 1:5), '"probs" must have a length of at least one.')
  expect_error(geoquantile(1:5, probs = c("green", "blue")), '"probs" must be numeric')
  expect_error(geoquantile(1:5, probs = TRUE), '"probs" must be numeric')
})

test_that("geoquantiles error if provided with types outside [1,9]", {
  expect_error(geoquantile(1:5, type = 10), '"type" must be a numeral between 1 and 9')
  expect_error(geoquantile(1:5, type = -1), '"type" must be a numeral between 1 and 9')
  expect_error(geoquantile(1:5, type = c("green", "blue")), '"type" must be numeric')
  expect_error(geoquantile(1:5, type = TRUE), '"type" must be numeric')
})
