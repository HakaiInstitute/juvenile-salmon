library(googlesheets)
library(lubridate)
library(tidyverse)
library(dplyr)

db <- gs_key("1hTC60Nc60k23rMMzcV9clPo4mcR1iqQYgKR2zD6nOfQ", visibility = "private", lookup = FALSE)
surveys <- gs_read(db, ws = "survey_data")
seines <- gs_read(db, ws = "seine_data")
packages <- gs_read(db, ws = "package_data")
fish <- gs_read(db, ws = "fish_field_data")

ss <- left_join(seines, surveys, by = "survey_id") %>% 
  select(seine_id, site_id, survey_date)
packages_ss <- left_join(packages, ss, by = "seine_id") %>% 
  mutate(date_site_sp = paste(survey_date, site_id, pkg_species, sep = "-"))

vf_wb <- gs_key("1sCmrDjQFGO4WOATRsTPjGaBgzMVZiU_ea1RZBUP3rws", visibility = "private", lookup = FALSE)
vf_ws <- gs_read(vf_wb, ws = "Sheet 1")

vf_fish <- vf_ws %>% 
  mutate(date_site_sp = paste(survey_date, site_id, species, sep = "-")) %>% 
  filter(status == "not dissected") %>% 
  group_by(date_site_sp) %>% 
  summarize(count = n())

vf_fish_pkgs <- left_join(vf_fish, packages_ss)
