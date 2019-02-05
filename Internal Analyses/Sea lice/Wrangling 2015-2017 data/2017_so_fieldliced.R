library('tidyverse')
library('lubridate')
library('googlesheets')

database_wip <- gs_title("Database WIP")
seine_data <- gs_read(database_wip, ws = "seine_data")
survey_data <- gs_read(database_wip, ws = "survey_data")
fish_field_data <-  gs_read(database_wip, ws = "fish_field_data")
sealice_field_data <- gs_read(database_wip, ws = "sealice_field")

survey_seines <- full_join(survey_data, seine_data, by = "survey_id")
fish <- left_join(fish_field_data, survey_seines) %>% 
  select(ufn, species, package_id, survey_id, seine_id, survey_date, site_id, region, zone, analysis_planned)
fish_lice <- left_join(sealice_field_data, fish) %>% 
  filter(analysis_planned == "SEMSP")

write.csv(fish_lice,"fish_lice_field_retained.csv")