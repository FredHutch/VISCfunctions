context("update_internal_processing")

# test .validate_sha()
test_that(".validate_sha() exits with descriptive errors when error conditions arise", {
  expect_error(update_visc_data(sha = "0"), "The provided commit SHA is not unique. Please use a longer partial SHA.")
  expect_error(update_visc_data(sha = "d1111bddd6"),
               "The provided commit SHA does not appear in the commit history. Please verify it and try again\\.")
})
