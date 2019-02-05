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
fish <- gs_read(db, ws = "fish_field_data")

surveys_seines <- full_join(surveys, seines, by = "survey_id")
fish_survey_seines <- left_join(fish, surveys_seines, by = "seine_id") %>%
  filter(analysis_planned == "SEMSP")

so_2018_coho_chinook <- filter(fish_survey_seines, species == "SO" & survey_date >= "2018-05-01" | species == "CO" | species == "CK") %>% 
  mutate(survey_year = year(survey_date))

inventory <-
  gs_key(
    "1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs",
    lookup = F,
    visibility = "private"
  )
dna <- gs_read(inventory, ws = "dna_metadata")
finclip <- gs_read(inventory, ws = "finclip_metadata")
samples <- rbind(dna, finclip)

available <- left_join( so_2018_coho_chinook, samples, by = "ufn") %>% 
  filter(sample_type == "DNA" | sample_type == "fin clip") %>% 
  group_by(species, survey_year, region, sample_type) %>% 
  summarize(count = n())
write.csv(available, "GSI samples available.csv")

so_2018_coho_chinook_dissection_status <- so_2018_coho_chinook %>% 
  group_by(species, survey_year, region, dissection_status) %>% 
  summarize(count = n())
write.csv(so_2018_coho_chinook_dissection_status, "Sockeye 2018, coho, and chinook dissection status.csv")
