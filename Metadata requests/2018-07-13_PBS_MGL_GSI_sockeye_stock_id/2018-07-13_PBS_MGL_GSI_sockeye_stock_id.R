library(googlesheets)
library(tidyverse)
library(hakaisalmon)


gsi_book <- gs_key("1W8Uc02Wg7QwVe4S0PNy03Pav6QQGngWIJJoTiAODxq8", lookup = F,
                   visibility = "private")

gsi <- gs_read(gsi_book, ws = "Sheet1") 

gsi_we <-left_join(gsi, survey_seines_fish) %>% 
  select(ufn, species, survey_date, site_id, sample_id, sample_location) %>% 
  drop_na(site_id)

labbook_2017 <- gs_key("1zFL-bMWL5O5PfoATjYHJACsORKVKlyfg3bg4YqK3kRQ", lookup = F,
                       visibility = "private")

fish_lab <- gs_read(labbook_2017, ws = "fish_lab_data") %>% 
  right_join(gsi) %>% 
  filter(date_processed > 2018-07-10) %>% 
  select(ufn, semsp_id, sample_id, sample_location) %>% 
  left_join(survey_seines_fish) %>% 
  select(sample_id, semsp_id, ufn) %>% 
  separate(semsp_id, c("year", "month", "day", "site")) %>% 
  unite(date, c("year", "month", "day"), sep = "-") %>% 
  mutate(species = "SO", region = "DI", julian_date = lubridate::yday(date)) %>% 
  write_csv(here::here("Metadata requests", "2018-07-13_PBS_MGL_GSI_sockeye_stock_id",
                       "2018-07-13_Hakai_PBS_MGL_sockeye_stock_id.csv"))

