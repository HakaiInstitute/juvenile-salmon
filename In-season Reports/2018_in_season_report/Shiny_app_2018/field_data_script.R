library(googlesheets)
library(tidyverse)
library(lubridate)

field_2018 <- gs_key("1iRQCKMTznHbMeI9GunbsgPyb-rtWgwj-0IfxLw7NBJE", 
                     lookup = FALSE, visibility = "private")

survey_data <- gs_read(field_2018, ws = "survey_data") %>% 
  drop_na(survey_id)

survey_data <- survey_data %>% 
  mutate(sampling_week = as.numeric((yday(survey_date) + 4) %/% 7))

survey_data$sampling_week <- recode_factor(survey_data$sampling_week, 
                                           `18` = "May 5",
                                           `19` = "May 12" ,
                                           `20` = "May 19", 
                                           `21` = "May 26",
                                           `22` = "June 2",
                                           `23` = "June 9", 
                                           `24` = "June 16", 
                                           `25` = "June 23",
                                           `26` = "June 30", 
                                           `27` = "July 6", 
                                           `28` = "July 13")

seine_data <- gs_read(field_2018, ws = "seine_data")

survey_seines <- left_join(seine_data, survey_data) %>% 
  mutate(year = year(survey_date))

saveRDS(survey_seines, here::here("In-season Reports", "2018_in_season_report", "Shiny_app_2018",
                   "data", "survey_seines.RDS"))

# fish_and_sealice_field_data <- gs_read(field_2018, 
#                                        ws = "fish_and_sealice_field_data")
# 
# fish_and_sealice_field_data <- left_join(fish_and_sealice_field_data, 
#                                          survey_seines, by = "seine_id")
# 
# # Get oceanoraphy data
# oceanography_metadata <- gs_read(field_2018, ws = "ctd_data") %>% 
#   drop_na(ctd_cast_id)
# 
# di_ctd_data <- read_csv(here("In-season Reports", "2018_in_season_report",
#                              "data", "di_ctd_data.csv"))
# 
# js_ctd_data <- read_csv(here("In-season Reports", "2018_in_season_report",
#                              "data", "js_ctd_data.csv"))
# 
# qu29 <- read_csv(here("In-season Reports", "2018_in_season_report",
#                       "data", "qu29.csv"))
# 
# qu39 <- read_csv(here("In-season Reports", "2018_in_season_report",
#                       "data", "qu39.csv"))
# 
# # Create primary key to join ctd data with fish field data
# di_ctd_data$ctd_cast_id <- paste(di_ctd_data$station, as_date(di_ctd_data$start_dt), di_ctd_data$cast_number, sep = "_")
# js_ctd_data$ctd_cast_id <- paste(js_ctd_data$station, as_date(js_ctd_data$start_dt), js_ctd_data$cast_number, sep = "_")
# 
# # Join survey and oceanography data
# survey_oceanography <- left_join(survey_data, oceanography_metadata)
