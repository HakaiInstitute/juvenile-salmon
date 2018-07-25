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

#Some of the fish from 2017 have been dissected but that data have not been
# put into the database and the WIP, nor the hakaisalmon package
sealice_not_in_WIP_book <- gs_key("1zFL-bMWL5O5PfoATjYHJACsORKVKlyfg3bg4YqK3kRQ",
                                  lookup = FALSE, visibility = "private")

sealice_not_in_WIP <- gs_read(sealice_not_in_WIP_book, ws = "sealice_lab") %>%
  mutate(
    motile_caligus = cm_lab + cpaf_lab + caf_lab + cgf_lab + ucal_lab,
    motile_lep = lpam_lab + lpaf_lab + lam_lab + laf_lab + lgf_lab + ulep_lab
  ) %>% 
  select(
    ufn, motile_caligus, motile_lep
  ) 
    
sealice_15_17 <- left_join(sealice_15_17, sealice_not_in_WIP, by = "ufn") %>% 
  replace_na(list(motile_caligus.y = 0, motile_lep.y = 0)) %>%
  mutate(motile_caligus = motile_caligus.x + motile_caligus.y,
         motile_lep = motile_lep.x + motile_lep.y) %>% 
  select(-motile_caligus.x, -motile_caligus.y, -motile_lep.x, -motile_lep.y)

sealice_time_series <- rbind(sea_lice_2018_summary, sealice_15_17) %>% 
  drop_na() %>% 
  mutate(year = year(survey_date), sampling_week = as.numeric((yday(survey_date) + 4) %/% 7)) %>% 
  filter(sampling_week <= 28, species %in% c("SO", "PI", "CU")) %>% 
  filter(site_id %in% c("D07", "D09", "D22", "D27", "D10", "D08", "D34",
                        "D20", "J03", "J02", "J09", "J11")) %>% 
  gather(`motile_caligus`, `motile_lep`, key = louse_species, value = n_lice)

sealice_time_series <- sealice_time_series %>% 
  mutate(sampling_week = recode_factor(sealice_time_series$sampling_week, `18` = "May 5", `19` = "May 12" ,
                                       `20` = "May 19", `21` = "May 26", `22` = "June 2",
                                       `23` = "June 9", `24` = "June 16", `25` = "June 23",
                                       `26` = "June 30", `27` = "July 6", `28` = "July 13"))

# Generate sealice prevalence column
motile_infected_hosts <- sealice_time_series %>%
  filter(n_lice > 0, species %in% c("SO", "PI", "CU")) %>% 
  group_by(year, region, species, sampling_week, louse_species) %>%
  summarise(n_infected = n())

hosts <- sealice_time_series %>%
  group_by(year, region, species, sampling_week, louse_species) %>%
  summarise(n_examined = n()) %>%
  filter(species %in% c("SO", "PI", "CU")) 

summary_sealice <- left_join(hosts, motile_infected_hosts) %>% 
  replace_na(list(n_infected = 0)) %>% 
  mutate(prevalence = n_infected /  n_examined) %>% 
  select(-n_infected, -n_examined)

# Generate abundance column for summary_sealice

abundance <- sealice_time_series %>% 
  select(year, region, species, sampling_week, louse_species, n_lice) %>% 
  group_by(year, region, species, sampling_week, louse_species) %>%
  summarise(abundance = mean(n_lice, na.rm = T)
            )

summary_sealice <- left_join(summary_sealice, abundance)

# Generate intensity column
intensity <- sealice_time_series %>% 
  filter(n_lice > 0) %>% 
  select(year, region, species, sampling_week, louse_species, n_lice) %>% 
  group_by(year, region, species, sampling_week, louse_species) %>%
  summarise(intensity =  mean(n_lice, na.rm = T)
            )

summary_sealice <- left_join(summary_sealice, intensity)

saveRDS(summary_sealice, here::here("In-season Reports", "2018_in_season_report", "Shiny_app_2018",
                                               "data", "summary_sealice.RDS"))


##### Read in CTD data 
## Get CTD data from EIMS database using R API
library(tidyverse)
library(hakaiApi)
client <- hakaiApi::Client$new()

qu39_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=QU39&limit=-1")
qu39_all <- client$get(qu39_endpoint) %>% 
  mutate(year = year(start_dt), date = as_date(start_dt), yday = yday(start_dt)) 

qu29_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=QU29&limit=-1")
qu29_all <- client$get(qu29_endpoint) %>% 
  mutate(year = year(start_dt), date = as_date(start_dt), yday = yday(start_dt))

js2_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=JS2&limit=-1")
js2_all <- client$get(js2_endpoint)  %>% 
  mutate(year = year(start_dt), date = as_date(start_dt), yday = yday(start_dt))


js12_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=JS12&limit=-1")
js12_all <- client$get(js12_endpoint)  %>% 
  mutate(year = year(start_dt), date = as_date(start_dt), yday = yday(start_dt))

js2_12_all <-rbind(js2_all, js12_all)

js2_12_all$station <- "js2_12"
  
# Create time series of average conditions which excludes the current year, using a loess function
# Do this using base R so thatI can extract the loess predictions to include in data frame to provide the value for
# which to test if the current years value falls above or below, given a date.
ctd_pre_2018 <- rbind(qu39_all, qu29_all, js2_12_all) %>% 
  mutate(year = year(start_dt), date = as_date(start_dt), yday = yday(start_dt),
         week = week(start_dt)) %>%
  filter(year < 2018, depth <= 30) %>% 
  select(year, date, week, yday, station, conductivity, temperature, depth, salinity, 
         dissolved_oxygen_ml_l) %>% 
  group_by(station, week, yday) %>% 
  summarise(mean_temp = mean(temperature, na.rm = T), 
            mean_do = mean(dissolved_oxygen_ml_l, na.rm = T),
            mean_salinity = mean(salinity, na.rm = T))

#Create current year data to compare to time series
ctd_post_time_series <- rbind(qu39_all, qu29_all, js2_12_all) %>%  
  filter(year == 2018, yday > 32, yday < 213,  depth <= 30) %>% 
  select(year, date, yday, station, conductivity, temperature, depth, salinity, 
         dissolved_oxygen_ml_l) %>% 
  group_by(station, yday) %>% 
  summarise(mean_temp = mean(temperature, na.rm = T), 
            mean_do = mean(dissolved_oxygen_ml_l, na.rm = T),
            mean_salinity = mean(salinity, na.rm = T))

#saveRDS(js2_12, here::here("In-season Reports", "2018_in_season_report",
 #                          "Shiny_app_2018", "data", "js2_12.RDS"))

## test out sst anomaly plot

## QU39
qu39_average <- ctd_pre_2018 %>% 
  filter(station == "QU39")

# Filter down to station of interest
qu39_this_year <- ctd_post_time_series %>% 
  filter(station == "QU39")

temp.lo_qu39 <- loess(mean_temp ~ yday, qu39_average, SE = T)

#create table for predicitions from loess function
sim_temp_data_qu39 <- tibble(yday = seq(min(qu39_average$yday), max(qu39_average$yday), 0.1))
#Predict temp in 0.1 day increments to provide smooth points to join
sim_temp_data_qu39$predicted_mean_temp <- predict(temp.lo_qu39, sim_temp_data_qu39, SE = T)

#Join simulated/predicted temp values from loess function to, observations from this year
qu39_temp_anomaly_data <- left_join(sim_temp_data_qu39, qu39_this_year) %>% 
  mutate(diff = if_else(mean_temp > predicted_mean_temp, "pos", "neg")) %>% 
  drop_na(diff) %>% 
  add_row(yday = (44+37)/2, predicted_mean_temp = predict(temp.lo_qu39, (44+37)/2), mean_temp = predict(temp.lo_qu39, (44+37)/2)) %>% 
  add_row(yday = 44.5, predicted_mean_temp = predict(temp.lo_qu39, 44.5), mean_temp = predict(temp.lo_qu39, 44.5)) %>% 
  add_row(yday = 124, predicted_mean_temp = predict(temp.lo_qu39, 124), mean_temp = predict(temp.lo_qu39, 124)) %>% 
  add_row(yday = (192 + 177) / 2, predicted_mean_temp = predict(temp.lo_qu39, (192 + 177) / 2), mean_temp = predict(temp.lo_qu39,(192 + 177) / 2))

# Create a linear interpolation of points that have zero difference between 
# loess model and 'observed data' so that an area plot will look right



# Create min and max for any given day of the time series
qu39_min <- qu39_average %>% group_by(yday) %>% filter(mean_temp == min(mean_temp))
qu39_max <- qu39_average %>% group_by(yday) %>% filter(mean_temp == max(mean_temp))


## Plot it
ggplot(data = qu39_temp_anomaly_data, aes(x = yday, y = mean_temp)) +
  geom_point(aes(x = yday, y = predicted_mean_temp))+
  geom_line(aes(x = yday, y = predicted_mean_temp)) +
  geom_ribbon(data = subset(qu39_temp_anomaly_data, mean_temp >= predicted_mean_temp), aes(ymin = predicted_mean_temp, ymax = mean_temp), fill = 'red', size = 4)+
  geom_ribbon(data = subset(qu39_temp_anomaly_data, mean_temp <= predicted_mean_temp), aes(ymin = mean_temp, ymax = predicted_mean_temp), fill = 'blue', size = 4)+
  theme_bw() +
  geom_smooth(data = qu39_average, aes(x = yday, y = mean_temp), size = 2, colour = 'black', se = T) +
  geom_point(data = qu39_min,
              aes(x = yday, y = mean_temp))+
  geom_point(data = qu39_max,
             aes(x = yday, y = mean_temp)) + 
  scale_x_continuous(breaks = (c(32, 60, 91, 121, 152, 182, 213)),
                    labels = (c("Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", 
                                "July 1", "Aug 1"))) +
  labs(x = "Date", y = "Temperature [°C]") +
  coord_cartesian(xlim = c(32,213))
  
##QU29


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
