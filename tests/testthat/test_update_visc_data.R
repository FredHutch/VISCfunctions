context("update_visc_data")

# test .validate_wd()
test_that(".validate_wd() function works when given a valid path.", {
  expect_match(.validate_wd(), "VISCfunctions/data$")
})

test_that(".validate_wd() generates descriptive errors if not calling function from within VISCfunctions repo", {
  tmp <- file.path(tempfile(pattern="tmp"))
  dir.create(tmp, recursive=TRUE)

  # error if not a git repo
  expect_error(.validate_wd(tmp), "Working directory must be inside a git repo when calling update_visc_data().")

  # error if git repo doesn't contain an R package
  repo <- git2r::init(tmp)
  expect_error(.validate_wd(tmp), "The current repo does not appear to be an R package.")

  # error if git repo and R package but not called VISCfunctions
  old <- getwd()
  setwd(tmp)
  package.skeleton("testpackage")
  setwd("testpackage")
  repo <- git2r::init(".")
  expect_error(.validate_wd("."), "At this time, update_visc_data can only be used to copy data to the VISCfunctions package.")
  setwd(old)
})

# test update_visc_data()
test_that("update_visc_data() messages fire as expected", {
  expect_message(update_visc_data(), ".+Success.+")
  expect_message(update_visc_data(sha = "d8859bdcb6"), "No .rda files to copy!")
})

