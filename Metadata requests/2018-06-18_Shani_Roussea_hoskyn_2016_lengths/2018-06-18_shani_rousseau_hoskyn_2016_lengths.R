# Data request for Shani Rousseau. Looking for Hoskyn D07 cathes in 2016

library(hakaisalmon)
library(tidyverse)
library(lubridate)

survey_seines %>% 
  filter(site_id == "D07", year(survey_date) == 2016, collection_protocol == "SEMSP") %>% 
  select(survey_date, seine_id, site_id, site_name, set_time, lat, lon, so_total, pi_total, cu_total, co_total,
         he_total) %>% 
  drop_na() %>% 
  write_csv("~/Google Drive File Stream/Team Drives/Juvenile Salmon Program/Data/2018-06-18_shanni_rousseau.csv")

survey_seines_fish %>% 
  filter(site_id == "D07", year(survey_date) == 2016, collection_protocol == "SEMSP") %>% 
  select(ufn, seine_id, survey_date, site_id, site_name, set_time, lat, lon, fork_length) %>% 
  drop_na() %>% 
  write_csv("~/Google Drive File Stream/Team Drives/Juvenile Salmon Program/Data/2018-06-18_shanni_rousseau_lengths.csv")
