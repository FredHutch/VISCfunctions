library(data.table)
library(dplyr)
library(tidyr)
library(testthat)
library(usethis)
# for reading .gz directly with fread()
library(R.utils)

gzf <- 'flow_and_sequences.csv.gz'
if (! file.exists(file.path('data-raw', gzf))){
  download.file(
    file.path(
      'https://github.com/SchiefLab/G001/raw/main/data/figures/flow_summary',
      gzf
    ),
    destfile = file.path('data-raw', gzf)
  )
}

fas <- fread('data-raw/flow_and_sequences.csv.gz')
# ubs <- fread('unblinded_sequences.csv')

# check that we have one row per ppt + visit
fas %>%
  group_by(PubID, Visit) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  unique %>%
  expect_equal(1)

non_numeric <- grep('^Number of|^Percent of', names(fas), value = TRUE, invert = TRUE)
keep_numeric <- c(
  # cell counts
  'Number of B cells',
  'Number of IgD-IgG+ B cells',
  'Number of IgD-IgG+ B cells that are GT8++ (without regard to KO binding status)',
  'Number of epitope-specific (KO-GT8++) IgG+ B cells',
  'Number of epitope-specific (KO-GT8++) IgG+ B cells that have BCR heavy and light chains sequenced',
  'Number of epitope-specific (KO-GT8++) sequenced IgG BCRs that are VRC01-class',

  # key percentages
  'Percent of B cells that are GT8++ (without regard to KO binding status)',
  'Percent of B cells that are epitope-specific (KO-GT8++)',
  'Percent of IgG+ B cells that are GT8++ (without regard to KO binding status)',
  'Percent of IgG+ B cells that are epitope-specific (KO-GT8++)',
  'Percent of GT8++IgG+ B cells that are KO-',
  'Percent of B cells detected as VRC01-class',
  'Percent of IgG+ B cells detected as VRC01-class',
  'Percent of GT8++ IgG+ B cells detected as VRC01-class',
  'Percent of epitope-specific (KO-GT8++) sequenced IgG BCRs that are VRC01-class'
)

drop_cols <- c(
  "V1",
  "Response",
  "Response (missing seq to 0)",
  "Response not VRC01",
  "Response not VRC01 (missing seq to 0)",
  "Sequence Performed",
  "root_Population",
  "PBMC_Population",
  "IgD_IgG_Population",
  "IgD_Population",
  "Bulk_Population",
  "IgG_KOneg_Population",
  "AG_Specific_Population",
  "IgD_IgG_GT8pospos_noKO_Population",
  "IgD_IgG_GT8pospos_KOneg_Population",
  "IgD_GT8pospos_noKO_Population",
  "IgD_GT8pospos_KOneg_Population",
  "Response_Ep_Specific",
  "Response_GT8"
)

df <- fas %>%
  # discard unneeded numeric (Percent* / Number*) columns
  select(
    all_of(
      intersect(
        names(fas),
        c(non_numeric, keep_numeric)
      )
    )
  ) %>%
  select(! all_of(drop_cols)) %>%
  # subset on and then remove a confusingly named column
  filter(BCell_Population == "/Lymphocytes/Singlets/Live|Dump-/CD19+CD20+") %>%
  select(-BCell_Population) %>%
  # from kellie's processing, lightly edited
  pivot_longer(
    c(
      matches('^Percent of'),
      matches('^Number')
    ),
    names_to = 'name',
    values_to = 'value'
  ) %>%
  mutate(value_type = case_when(
    grepl('^Percent', name) ~ 'percent',
    grepl('^Number', name) ~ 'count',
  )) %>%
  mutate(
    cell_population = name,
    cell_population = sub('Number of ', '', cell_population),
    # for a typo in original dataset
    cell_population = sub('Number ', '', cell_population),
    cell_population = sub('Percent of .+ (that are|detected as) ', '', cell_population)
  ) %>%
  mutate(
    percent_denominator = if_else(
      value_type == 'percent',
      gsub('^Percent of | ((that are)|(detected as)) .+$', '', name),
      NA_character_
    ),
    percent_denominator = case_match(
      percent_denominator,
      'GT8++IgG+ B cells' ~ 'GT8++ IgG+ B cells',
      "epitope-specific (KO-GT8++) sequenced IgG BCRs" ~ "sequenced epitope-specific (KO-GT8++) IgG+ B cells",
      .default = percent_denominator
    ),
    cell_population = case_match(
      cell_population,
      "epitope-specific (KO-GT8++) sequenced IgG BCRs that are VRC01-class" ~ 'VRC01-class',
      "KO-" ~ 'GT8++KO-',
      "IgD-IgG+ B cells" ~ 'IgG+',
      "IgD-IgG+ B cells that are GT8++ (without regard to KO binding status)" ~ 'GT8++ IgG+',
      "epitope-specific (KO-GT8++) IgG+ B cells that have BCR heavy and light chains sequenced" ~ "sequenced epitope-specific (KO-GT8++) IgG+ B cells",
      .default = gsub(' \\(without regard to KO binding status\\)|IgD\\-', '', cell_population)
    ),
    bcell_population = cell_population,
    igx_type = if_else(grepl('IgG', name), 'IgG+', NA_character_),
    Group = NA_character_,
    dose = NA_character_,
    dose_unit = NA_character_,
    visitno = sub('^V', '', Visit),
    visit = NA_character_,
    visitunits = NA_character_,
    antigen_specificity = if_else(grepl('GT8[+][+]|VRC01[-]class', name), 'GT8++', NA_character_),
    epitope_specificity = if_else(grepl('KO[-]|VRC01[-]class', name), 'KO-', NA_character_),
    bnab_class = if_else(grepl('^VRC01[-]class$', cell_population), 'VRC01-class', NA_character_),
    source_assay = if_else(grepl('VRC01[-]class|sequenced', name), 'sequencing', 'flow'),
    flag_bound = NA,
    probeset = 'G001 PBMC (KO11 eOD-GT8)',
    sample_type = 'PBMC',
    source_file = 'https://github.com/SchiefLab/G001/raw/main/data/figures/flow_summary/flow_and_sequences.csv.gz',
    PubID = sub('^PubID_', '', PubID)
  ) %>%
  # column cleanup
  select(
    -cell_population, -Visit
  ) %>%
  # Select/rename/reorder columns
  select(
    PubID,
    Group,
    Treatment,
    dose,
    dose_unit,
    visitno,
    visit,
    visitunits,
    Week = weeks_post,
    bcell_population,
    igx_type,
    antigen_specificity,
    epitope_specificity,
    bnab_class,
    source_assay,
    flag_bound,
    probeset,
    sample_type,
    source_file,
    name,
    value,
    value_type,
    percent_denominator
  )

G001_fas <- df

usethis::use_data(G001_fas)
