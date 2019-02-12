library(tidyverse)
library(dplyr)
library(googlesheets)

db <- gs_key("1hTC60Nc60k23rMMzcV9clPo4mcR1iqQYgKR2zD6nOfQ", visibility = "private", lookup = FALSE)
inventory <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", visibility = "private", lookup = FALSE)
dna <- gs_read(inventory, ws = "dna_metadata")
fish <- gs_read(db, ws = "fish_field_data")
survey <- gs_read(db, ws = "survey_data")
seines <- gs_read(db, ws = "seine_data")
survey_seines <- left_join(survey, seines, by = "survey_id")
fish_survey_seines <- left_join(fish, survey_seines, by = "seine_id")

dna_gsi <- filter(dna, container_id == "BDN7")

fish_dna <- left_join(dna_gsi, fish_survey_seines, by = "ufn") %>%
  select(ufn, semsp_id, species, survey_id, seine_id, survey_date, site_id, region, zone, site_name, container_id, container_cell, sample_location, comments_sample)

write.csv(fish_dna, here::here("2018-09-27_2015-D08_D09_SO_DNA_samples_GSI.csv"))
          
          