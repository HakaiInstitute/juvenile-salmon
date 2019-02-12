library(hakaisalmon)
library(tidyverse)
library(lubridate)

fishies <- survey_seines_fish %>% 
  mutate(year = year(survey_date)) %>% 
  filter(species %in% c("PI", "CU"), dissection_status == "dissected",
         year %in% c("2015", "2016")) %>% 
  left_join(stomach_metadata) %>% 
  left_join(sample_container_inventory) %>% 
  select(ufn, survey_date, site_id, species, dissection_status, sample_id, container_id,
         current_location) %>% 
  write_csv(here::here("Dissected_2015_2016_PI_CU.csv"))

            