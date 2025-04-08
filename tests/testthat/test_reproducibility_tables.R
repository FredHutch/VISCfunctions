context("reproducibility_tables")


test_that("get_full_name() testing", {

  # Garbage username should just return username
  expect_equal(object = get_full_name('fakeuser'), expected = 'fakeuser')


})


test_that("get_session_info() testing", {

  # Specifics change depending on user specifics and environment/packages loading, but can check some details
  temp_session_info <- get_session_info()

  # testing dimension
  nrow_expected <- length(sessioninfo::session_info()[[1]]) +
    ifelse(any(temp_session_info$platform_table$name == "repo"), 4, 3)
  expect_equal(object = dim(temp_session_info$platform_table), expected = c(nrow_expected,2))

  ncol_expected <- ifelse(any(colnames(temp_session_info$packages_table) == "data.version"), 5, 4)
  expect_equal(object = ncol(temp_session_info$packages_table), expected = ncol_expected)

  # test libpath column option produces that column
  expect_true('libpath' %in% names(get_session_info(libpath = TRUE)$packages_table))

  ## testing some outputs from sessioninfo::session_info()
  expected_session_info <- sessioninfo::session_info()

  # Comparing platform
  expected_platform <- data.frame(
    name = names(expected_session_info$platform),
    value = matrix(unlist(expected_session_info$platform), nrow = length(expected_session_info$platform)),
    stringsAsFactors = FALSE)
  expect_equal(object = temp_session_info$platform_table[match(expected_platform$name, temp_session_info$platform_table$name), ], expected = expected_platform)

  # Comparing packages
  expected_packages <- expected_session_info$packages[expected_session_info$packages$attached,]
  expected_packages <- data.frame(package = expected_packages$package,
                                 version = expected_packages$loadedversion,
                                 date = expected_packages$date,
                                 source = expected_packages$source,
                                 stringsAsFactors = FALSE) %>%
  # need to re-shorten git hash after source is extracted from the object of class `packages_info`
  dplyr::mutate(source = sub('([@][0-9a-f]{7})[0-9a-f]{33}', '\\1', source))

  expect_equal(object = temp_session_info$packages_table, expected = expected_packages)

})

