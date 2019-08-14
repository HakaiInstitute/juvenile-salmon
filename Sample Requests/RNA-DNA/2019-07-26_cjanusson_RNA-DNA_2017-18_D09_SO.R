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

# Rerunning the code will overwrite the records of the .csv generated below because samples are selected at random. FOR THE FUTURE, USE set.seed(x) where x is some number, to enable
# reproducible results (the same numbers will be randomly selected every time)

write.csv(so_rna_rand, "2019-07-26_cjanusson_2017-18_D09_SO_subsample.csv")

# 2019-08-14 update:
# Sampling with replacement was meant to enable sampling seines in which n < 5; however, this meant that fish were double-sampled also in seines where n >=5 and removed with the
# distinc() function. This led to some seines being under-represented in the subsample list. I had to go back and randomly sample additional fish to fill out the seines that should
# have at least 5 samples.

samp1 <- read.csv("Sample Requests/RNA-DNA/2019-07-26_cjanusson_2017-18_D09_SO_subsample.csv") %>% 
  group_by(seine_id) %>% 
  select(-1)

# Summarize the # of fish per seine from the original subsample list
samp1_summ <- samp1 %>% 
  summarize(countsamp1=n())

# Summarize the # of fish per seine from the total avilable list of specimens
so_d09_17_18_summ <- so_d09_17_18 %>% 
  group_by(seine_id) %>% 
  summarize(count = n())

# Identify which seines in the subsample list had fewer than 5 fish selected due to being double-sampled
compare <- left_join(so_d09_17_18_summ, samp1_summ) %>% 
  filter(countsamp1 < 5 & count >= 5) %>% 
  mutate(n_needed = 5 - countsamp1)

library(purrr)

# Get all fish that are part of the seines identified above, excluding fish that are already part of the original subsample list.
# Then, randomly sample n fish per seine
extra_fish <- inner_join(so_d09_17_18, compare) %>% 
  anti_join(samp1, by = "ufn")

set.seed(123)

samp2 <- extra_fish %>% 
  group_by(seine_id) %>% 
  nest() %>% 
  mutate(n = c(1,1,1,1,2)) %>% 
  mutate(samp = map2(data, n, sample_n)) %>% 
  select(seine_id, samp) %>% 
  unnest() %>% 
  select(-count, -countsamp1, -n_needed) %>% 
  left_join(rna_dna_loc, by = "ufn")

write.csv(samp2,"Sample Requests/RNA-DNA/2019-08-14_cjanusson_2017-18_D09_SO_subsample2.csv")

         