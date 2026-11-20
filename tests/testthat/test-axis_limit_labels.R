test_that("helpers label numeric extrema rather than vector positions", {
  expect_identical(add_gte_to_upper_limit(c(30, 0, 10)),
                   c("\u2265 30", "0", "10"))
  expect_identical(add_lte_to_lower_limit(c(30, 0, 10)),
                   c("30", "\u2264 0", "10"))
  expect_identical(add_lte_to_lower_limit(c(-5, 0, 5)),
                   c("\u2264 -5", "0", "5"))
})

test_that("ties and single finite breaks are labelled", {
  expect_identical(add_gte_to_upper_limit(c(2, 1, 2)),
                   c("\u2265 2", "1", "\u2265 2"))
  expect_identical(add_lte_to_lower_limit(c(1, 2, 1)),
                   c("\u2264 1", "2", "\u2264 1"))
  expect_identical(add_gte_to_upper_limit(2), "\u2265 2")
  expect_identical(add_lte_to_lower_limit(2), "\u2264 2")
})

test_that("empty and non-finite breaks are handled safely", {
  x <- c(NA_real_, NaN, -Inf, Inf)
  for (fun in list(add_gte_to_upper_limit, add_lte_to_lower_limit)) {
    expect_identical(fun(numeric()), character())
    expect_identical(fun(x), as.character(x))
  }
  expect_identical(add_gte_to_upper_limit(c(NA, 1, 2, Inf)),
                   c(NA_character_, "1", "\u2265 2", "Inf"))
})

test_that("invalid breaks fail clearly", {
  for (fun in list(add_gte_to_upper_limit, add_lte_to_lower_limit)) {
    expect_error(fun(c("1", "2")), "numeric vector")
    expect_error(fun(factor(1:2)), "numeric vector")
    expect_error(fun(matrix(1:4, 2)), "numeric vector")
  }
})

test_that("helpers work as ggplot2 scale labellers", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = c(0, 10, 20)),
                       ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(breaks = c(0, 10, 20),
                                labels = add_gte_to_upper_limit)
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$layout$panel_params[[1]]$y$get_labels(),
                   c("0", "10", "\u2265 20"))
})
