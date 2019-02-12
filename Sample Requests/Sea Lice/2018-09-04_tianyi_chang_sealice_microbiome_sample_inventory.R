library(lubridate)
library(tidyverse)
library(googlesheets)

db <-
  gs_key(
    "1hTC60Nc60k23rMMzcV9clPo4mcR1iqQYgKR2zD6nOfQ",
    lookup = F,
    visibility = "private"
  )
surveys <- gs_read(db, ws = "survey_data")
seines <- gs_read(db, ws = "seine_data")
fish_field <- gs_read(db, ws = "fish_field_data")
fish_lab <- gs_read(db, ws = "fish_lab_data")
sealice <- gs_read(db, ws = "sealice_lab_motiles")

surveys_seines <- full_join(surveys, seines, by = "survey_id")

fish_lab_field <- left_join(fish_lab, fish_field, by = "ufn") %>%
  filter(lice_collection_protocol == "lab_motiles_cryo") %>%
  filter(qc_flag == "N")

fish_survey_seines <-
  left_join(fish_lab_field, surveys_seines, by = "seine_id") %>%
  select(
    ufn,
    date_processed,
    species,
    weight,
    standard_length,
    fork_length,
    survey_id,
    survey_date,
    site_id,
    region,
    zone,
    site_name,
    seine_id,
    set_time,
    lat,
    lon
  )

fish_survey_seines_lice <- left_join(fish_survey_seines, sealice, by = "ufn")

samples <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", lookup = F, visibility = "private")
container_inventory <- gs_read(samples, ws = "sample_container_inventory") %>% 
  select(container_id, current_location, storage_unit)
sealice_samples <- gs_read(samples, ws = "sealice_metadata") %>% 
  filter(storage_medium == "frozen -80")
samples_inventory <- left_join(sealice_samples, container_inventory, by = "container_id")

fish_lice_samples <- left_join(samples_inventory, fish_survey_seines_lice, by = "ufn")
write.csv(fish_lice_samples, here::here("2018-09-04_tianyi_chang_sealice_microbiome_sample_inventory.csv"))
