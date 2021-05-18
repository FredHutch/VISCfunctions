## code to prepare `cds_datasets` dataset goes here

library(tidyverse)
library(DataSpaceR)
con <- connectDS()

cvd812 <- con$getStudy('cvd812')
CAVD812_mAB <- cvd812$getDataset("NAB Ig") %>%
  distinct(
    product = mab_name_source,
    virus = cds_virus_name,
    ic50 = titer_curve_ic50,
    ic80 = titer_curve_ic80
  ) %>% as.data.frame()

usethis::use_data(CAVD812_mAB, overwrite = TRUE)

