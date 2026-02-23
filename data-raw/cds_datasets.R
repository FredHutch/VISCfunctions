## code to prepare `cds_datasets` dataset goes here

library(tidyverse)
library(DataSpaceR)
con <- connectDS()

cvd812 <- con$getStudy('cvd812')
CAVD812_mAB <- cvd812$getDataset("NAB Ig") |>
  distinct(
    product = mab_name_source,
    virus = cds_virus_name,
    ic50 = titer_curve_ic50,
    ic80 = titer_curve_ic80
  ) |> as.data.frame()

usethis::use_data(CAVD812_mAB, overwrite = TRUE)

# bama dataset
cvd579 <- con$getStudy('cvd579')

cvd579_trt <- cvd579$getDataset("Demographics") |>
  left_join(cvd579$treatmentArm, join_by(study_arm == arm_name)) |>
  select(participant_id,
         study_start_date,
         study_type,
         study_group, #for blinded example code
         study_arm, #for unblinded example code
         study_arm_summary,
         description)

cvd579_bama <- cvd579$getDataset("BAMA") |>
  distinct(
    participant_id,
    specimen_type,
    assay_identifier,
    antibody_isotype,
    antigen,
    response_call,
    dilution,
    mfi_delta,
    mfi_raw,
    mfi_blank,
    mfi_bkgd,
    auc) |>
  left_join(cvd579_trt, join_by(participant_id)) |>
  as.data.frame()

usethis::use_data(cvd579_bama, overwrite = TRUE)

# ICS
# cvd579_bama <- cvd579$getDataset("ICS")
