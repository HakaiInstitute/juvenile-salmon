library(lubridate)
library(tidyverse)
library(googlesheets)
library(stringr)

db <- gs_key("1hTC60Nc60k23rMMzcV9clPo4mcR1iqQYgKR2zD6nOfQ", lookup = F, visibility = "private")
surveys <- gs_read(db, ws = "survey_data")
seines <- gs_read(db, ws = "seine_data")
fish <- gs_read(db, ws = "fish_field_data")
sealice <- gs_read(db, ws = "sealice_field")

survey_seines <- full_join(surveys,seines, by = 'survey_id')
survey_seines_fish <- left_join(fish,survey_seines, by = "seine_id")

fish_2018 <- survey_seines_fish %>% 
  filter(survey_date >= as.Date("2018-05-01")) %>% 
  select(ufn,survey_date,site_id)

so_2018 <- fish_2018 %>% 
  filter(species == "SO")

so_lice <- left_join(so_2018,sealice) %>% 
  filter(licing_protocol_field != "NA") %>% 
  mutate(motile_count = cal_mot_field + cgf_field + lpam_field + lpaf_field + lam_field + laf_field + lgf_field) %>% 
  filter(motile_count>0)
write.csv(so_lice, here::here("so_2018_lice.csv"))

fish_lice <- left_join(fish_2018,sealice) %>% 
  filter(licing_protocol_field != "NA") %>% 
  mutate(motile_count = cal_mot_field + cgf_field + lpam_field + lpaf_field + lam_field + laf_field + lgf_field) %>% 
  filter(motile_count>0)

write.csv(fish_lice, here::here("so_cu_pi_2018_lice.csv"))

fishlice_summary <- fish_lice %>% 
  filter(str_detect(site_id,"J") | site_id == "D22" | site_id == "D27") %>% 
  group_by(survey_date, site_id) %>% 
  summarise(count=n())
write.csv(fishlice_summary, here::here("lousyfish_summary_DP_JS.csv"))
