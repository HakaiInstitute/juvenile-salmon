library(googlesheets)
library(lubridate)
library(tidyverse)

# Data pulled from files used to create SQLite database (WIP)
fish <- read.csv('../jsp-data-packages/data/fish.csv')
seines <- read.csv('../jsp-data-packages/data/seines.csv')
surveys <- read.csv('../jsp-data-packages/data/surveys.csv')

# Inventory has yet to be migrated to eLabJournal
inventory <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", visibility = "private", lookup = FALSE)
containers <- gs_read(inventory, ws = "sample_container_inventory") %>% 
  select(-container_num, -container_grid_size)
rna_dna <- gs_read(inventory, ws = "rna-muscle_metadata")
rna_dna_loc <- left_join(rna_dna, containers, by = "container_id") %>% 
  select(ufn, sample_id, container_id, container_cell, sample_location, comments_sample, current_location, date_updated, 
         storage_unit, storage_tier_1, storage_tier_2)

ss <- left_join(seines,surveys, by = "survey_id") %>% 
  mutate(year = year(survey_date))

fish_ss <- left_join(fish,ss, by = "seine_id")

so_d09_17_18 <- fish_ss %>% 
  filter(year > 2016 & species == "SO" & site_id == "D09" & dissection_protocol != "irregular") %>% 
  select(ufn, species, seine_id, survey_date, site_id, date_processed,dissection_protocol, comments_protocol) %>% 
  drop_na(date_processed)

so_rna <- left_join(so_d09_17_18, rna_dna_loc, by = "ufn") %>% 
  group_by(seine_id)

so_rna_rand <- sample_n(so_rna, 5, replace = TRUE) %>% 
  distinct(ufn, .keep_all = TRUE)

write.csv(so_rna_rand, "2019-07-26_cjanusson_2017-18_D09_SO_subsample.csv")
