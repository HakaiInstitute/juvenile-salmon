library(tidyverse)
library(lubridate)
library(googlesheets)
library(here)

field_2019 <- gs_key("1cHgZszv--FlV207cwSpe9hFJipb7HhS4IW9vVrUNg8M", visibility = "private", lookup = FALSE)
surveys_2019 <- gs_read(field_2019, ws = "survey_data")
seines_2019 <- gs_read(field_2019, ws = "seine_data")
fish_2019 <- gs_read(field_2019, ws = "fish_field_data")

spc_lw_2019 <- fish_2019 %>% 
  select(ufn, species, survey_date, site_id, wt_field = weight, fl_field = fork_length) %>% 
  filter(species %in% c("SO", "PI", "CU")) %>% 
  mutate(K_field = (10^5*wt_field)/fl_field^3)

sites <- read.csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/sites.csv")
surveys <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv")
seines <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv")
fish_field <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_field_data.csv", guess_max = 10000)
fish_lab <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_lab_data.csv", guess_max = 10000)

spc_lw_2015_2018 <- left_join(fish_field, fish_lab, by = 'ufn') %>% 
  left_join(seines, by = 'seine_id') %>% 
  left_join(surveys, by = 'survey_id') %>%
  select(ufn, species, survey_date, site_id, wt_field = weight_field, fl_field = fork_length_field, wt_lab = weight, fl_lab = fork_length) %>% 
  filter(species %in% c("SO", "PI", "CU")) %>% 
  mutate(K_field = (10^5*wt_field)/fl_field^3) %>% 
  mutate(K_lab = (10^5*wt_lab)/fl_lab^3)

spc_lw_all <- bind_rows(spc_lw_2015_2018, spc_lw_2019) %>% 
  filter(!is.na(K_field) | !is.na(K_lab)) %>% 
  left_join(select(sites, site_id, region,zone), by = "site_id") %>% 
  select(ufn, species, survey_date, site_id, region, zone, wt_field, fl_field, K_field, wt_lab, fl_lab, K_lab)

write_csv(spc_lw_all, here("Metadata requests", "2019-10-01_Brian_Hunt_SPC_lengthweight", "2019-10-01_Brian_Hunt_SPC_lengthweight.csv"))

jg_subsample <- c("U20096", "U20127", "U20138", "U20228", "U20230", "U20234", "U20236", "U20582", "U20587", "U20592", "U20595", "U20598", "U20711", "U20712", "U20719", "U20749", "U20750", "U20831", "U20833", "U21211", "U21213", "U21214", "U21604", "U21609", "U21610")
  
fish_2019_jg <- fish_2019 %>% 
  filter(ufn %in% jg_subsample) %>% 
  select(1:7)
write_csv(fish_2019_jg, "2019-10-06_Jessica_Garzke_2019_subsample.csv")
