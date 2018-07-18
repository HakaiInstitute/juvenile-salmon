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


# Import 2018 sea lice data
fish_and_sealice_field_data <- gs_read(field_2018, 
                                       ws = "fish_and_sealice_field_data")

fish_and_sealice_field_data <- left_join(fish_and_sealice_field_data, 
                                         survey_seines, by = "seine_id")

saveRDS(fish_and_sealice_field_data, here::here("In-season Reports", "2018_in_season_report", "Shiny_app_2018",
                     "data", "fish_and_sealice_field_data.RDS"))

sea_lice_2018_summary <- fish_and_sealice_field_data %>%
  filter(species %in% c("SO", "PI", "CU")) %>% 
  drop_na(field_liced) %>% 
  mutate(
         motile_caligus = cm + cpaf + caf + cgf,
         motile_lep = lpam + lpaf + lam + laf + lgf
  ) %>% 
  select(ufn, survey_date, site_id, region, species, motile_caligus, motile_lep)

# Import motile time series data
sealice_15_17 <- hakaisalmon::sealice_lab_motiles %>%
  mutate(
    motile_caligus = cm_lab + cpaf_lab + caf_lab + cgf_lab + ucal_lab,
    motile_lep = lpam_lab + lpaf_lab + lam_lab + laf_lab + lgf_lab + ulep_lab
  ) %>% 
  left_join(hakaisalmon::survey_seines_fish) %>% 
  select(
    ufn, survey_date, site_id, region, species, motile_caligus, motile_lep
  )

sealice_time_series <- rbind(sea_lice_2018_summary, sealice_15_17) %>% 
  drop_na() %>% 
  mutate(year = year(survey_date), sampling_week = as.numeric((yday(survey_date) + 4) %/% 7)) %>% 
  filter(sampling_week <= 28) %>% 
  gather(`motile_caligus`, `motile_lep`, key = louse_species, value = n_lice)

sealice_time_series <- sealice_time_series %>% 
  mutate(sampling_week = recode_factor(sealice_time_series$sampling_week, `18` = "May 5", `19` = "May 12" ,
                                       `20` = "May 19", `21` = "May 26", `22` = "June 2",
                                       `23` = "June 9", `24` = "June 16", `25` = "June 23",
                                       `26` = "June 30", `27` = "July 6", `28` = "July 13"))

#TODO: Filter down sites comparable core stations for each year
#TODO: Calculate weekly average of all indices, and average those for the annual index 

# Generate sealice prevalence column
motile_infected_hosts <- sealice_time_series %>%
  group_by(year, region, species, louse_species) %>%
  filter(n_lice >= 1, species %in% c("SO", "PI", "CU")) %>% 
  summarise(n_infected = n())

hosts <- sealice_time_series %>%
  group_by(year, region, species, louse_species) %>%
  summarise(n_examined = n()) %>%
  filter(n_examined >= 3, species %in% c("SO", "PI", "CU")) 

summary_sealice <- left_join(hosts, motile_infected_hosts) %>% 
  replace_na(list(n_infected = 0)) %>% 
  mutate(prevalence = n_infected /  n_examined) %>% 
  select(-n_infected, -n_examined)

# Generate abundance column for summary_sealice

abundance <- sealice_time_series %>% 
  select(year, region, species, louse_species, n_lice) %>% 
  group_by(year, region, species, louse_species) %>%
  summarise(abundance = mean(n_lice, na.rm = T), 
            abundance_se = sd(n_lice, na.rm = T) / sqrt(n())
            )

summary_sealice <- left_join(summary_sealice, abundance)

# Generate intensity column
intensity <- sealice_time_series %>% 
  filter(n_lice > 0) %>% 
  select(year, region, species, louse_species, n_lice) %>% 
  group_by(year, region, species, louse_species) %>%
  summarise(intensity =  mean(n_lice, na.rm = T),
            itensity_se = sd(n_lice, na.rm = T)/sqrt(n())
            )

summary_sealice <- left_join(summary_sealice, intensity)

saveRDS(summary_sealice, here::here("In-season Reports", "2018_in_season_report", "Shiny_app_2018",
                                               "data", "summary_sealice.RDS"))
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
