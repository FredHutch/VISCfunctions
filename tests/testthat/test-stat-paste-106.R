test_that("existing stat_paste output and positional calls are preserved", {
  expect_identical(stat_paste(5), "5")
  expect_identical(stat_paste(5, 2), "5 (2)")
  for (b in c("(", "[", "{", "|")) {
    closing <- switch(b, "(" = ")", "[" = "]", "{" = "}", "|" = "|")
    expect_identical(stat_paste(5, 2, 8, bound_char = b),
                     paste0("5 ", b, "2, 8", closing))
  }
  expect_identical(stat_paste(5, 2, 8, 1, TRUE, "[", "; ", "NA", "%"),
                   "5.0% [2.0%; 8.0%]")
  expect_identical(stat_paste(NA), "---")
  expect_identical(stat_paste(NA, NA), "---")
  expect_identical(stat_paste(NA, NA, NA), "---")
})

test_that("only the nonmissing first statistic is bolded", {
  expect_identical(stat_paste(5, 2, 8, bold_first = TRUE,
                             output_type = "pandoc", bound_char = "["),
                   "**5** [2, 8]")
  expect_identical(stat_paste(5, bold_first = TRUE, output_type = "pandoc"),
                   "**5**")
  expect_identical(stat_paste(c(5, NA, NA), c(2, 2, NA),
                             bold_first = TRUE, output_type = "pandoc"),
                   c("**5** (2)", "--- (2)", "---"))
  expect_identical(stat_paste(NA, NA, NA, na_str_out = "missing",
                             bold_first = TRUE, output_type = "pandoc"), "missing")
  expect_identical(stat_paste(5, 2, digits = 1, suffix = "%",
                             bold_first = TRUE, output_type = "pandoc"),
                   "**5.0%** (2.0%)")
  expect_identical(stat_paste("estimate", "interval", bold_first = TRUE,
                             output_type = "pandoc"), "**estimate** (interval)")
})

test_that("HTML and LaTeX use text-level markup", {
  for (fmt in c("latex", "html")) {
    first <- kableExtra::text_spec("5", format = fmt, bold = TRUE, escape = FALSE)
    expect_identical(stat_paste(5, 2, bold_first = TRUE, output_type = fmt),
                     paste0(first, " (2)"))
    expect_identical(stat_paste(NA, NA, bold_first = TRUE, output_type = fmt),
                     "---")
  }
  expect_identical(stat_paste(5, 2, bold_first = TRUE),
                   stat_paste(5, 2, bold_first = TRUE, output_type = "latex"))
  expect_identical(stat_paste(5, 2, bold_first = TRUE, output_type = "no_markup"),
                   "5 (2)")
  expect_identical(stat_paste(5, 2, output_type = "html"), "5 (2)")
})

test_that("comma and dash delimiters have no closing bracket", {
  expect_identical(stat_paste(1, 5, bound_char = ","), "1, 5")
  expect_identical(stat_paste(1, 5, bound_char = "--"), "1--5")
  expect_identical(stat_paste(1, 5, 9, bound_char = ",", sep = "; "), "1, 5; 9")
  expect_identical(stat_paste(1, 5, 9, bound_char = "--", sep = "--"), "1--5--9")
  expect_identical(stat_paste(1, 5, bound_char = "--", bold_first = TRUE,
                             output_type = "pandoc"), "**1**--5")
  expect_identical(stat_paste(c(1, NA), c(NA, NA), bound_char = "--"),
                   c("1-----", "---"))
})

test_that("invalid new options are rejected", {
  for (bad in list(NA, c(TRUE, FALSE), "TRUE", 1, logical())) {
    expect_error(stat_paste(5, bold_first = bad), "bold_first")
  }
  expect_error(stat_paste(5, output_type = "invalid"), "arg")
  expect_error(stat_paste(5, bound_char = "invalid"), "arg")
})
