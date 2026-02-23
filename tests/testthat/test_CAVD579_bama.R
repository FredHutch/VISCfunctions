context("check_CAVD579_bama")

# test basic dataset details based on
# https://dataspace.cavd.org/cds/CAVD/app.view#learn/learn/Study/label=CAVD%20579
test_that("CAVD579_bama has expected ID, visit and antigen counts", {

  # Participants were enrolled in two groups
  expect_equal(length(unique(CAVD579_bama$study_group)),
               2)

  # Group 1: DCVax-001 prime/MVA-CMDR boost (n=8);
  # Group 2: placebo/MVA-CMDR (n=6)
  expect_equal(length(unique(CAVD579_bama$participant_id)),
               14)

  # Participants were administered 2 intramuscular injections of MVA-CMDR
  # at weeks 0 and 12, and followed for 36 weeks after second immunization (week 48).
  expect_equal(length(unique(CAVD579_bama$visit_day)),
               3)

  expect_equal(length(unique(CAVD579_bama$antibody_isotype)),
               5)

  expect_equal(length(unique(CAVD579_bama$antigen)),
               9)

  })
