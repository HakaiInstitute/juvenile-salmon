library(tidyverse)
library(googlesheets4)
library(here)
library(lubridate)

mdt_slug <- "1RLrGasI-KkF_h6O5TIEMoWawQieNbSZExR0epHa5qWI"

survey_data <- read_sheet(mdt_slug, sheet = "survey_data") 
1
seine_data <- read_sheet(mdt_slug, "seine_data")
fish_field_data <- read_sheet(mdt_slug, "fish_field_data")
fish_lab_data <- read_sheet(mdt_slug, "fish_lab_data")


mi_slug <- "1opdGSi-BIJtgCJgv2X-6-dBRFIFyOtVvvtjy9eQ18nc"
dna_samples <- read_sheet(mi_slug, sheet = "dna_samples") 

#2020 finclip whatman data
clips20 <- "1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk"
whatman_locs_20 <- read_sheet("1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk", sheet = "fin_clips")
clips21 <- "1IqjeOf2mTZYRhYscct6Ra5fIzp2ruK8E3Y1DxvsZ6V8"
whatman_locs_21 <- read_sheet("1IqjeOf2mTZYRhYscct6Ra5fIzp2ruK8E3Y1DxvsZ6V8", sheet = "fin_clips")
clips <- bind_rows(whatman_locs_20, whatman_locs_21)
  
gsi <- left_join(survey_data, seine_data) %>% 
  left_join(fish_field_data) %>% 
  left_join(fish_lab_data) %>% 
  left_join(dna_samples) %>% 
  left_join(clips) %>% 
  filter(species == "SO") %>% 
  mutate(year = year(survey_date),
         survey_date = ymd(survey_date)) %>% 
  filter(year %in% c(2020, 2021)) %>% 
  select(survey_date, ufn, sample_id, whatman_sheet, whatman_col, whatman_row, species, lat, long) %>% 
  drop_na(sample_id)

gsi$lat <- as.numeric(gsi$lat)
gsi$long <- as.numeric(gsi$long)

write_csv(gsi, here("hakai_sockeye_gsi_2020_2021.csv"))
