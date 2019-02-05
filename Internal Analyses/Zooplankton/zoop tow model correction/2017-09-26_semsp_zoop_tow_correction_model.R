# Model SEMSP zooplankton tow data

# Many instances of incorrect flowmeter readings exist from 2015 horizontal
#   zooplankton tows from when the flowmeter was not working. The problem was
#   correcte after 2015-06-04 when a ball bearing that was impeding rotor 
#   rotation was removed.
# For horizontal tows, there is no substitute for the flowmeter to be able
#   to calculate volume filtered. This script models the volume filtered of 
#   tows with wrong readings, using duration of tow and secchi depth as
#   variables in a multiple regression to model an equation for volume filtered,
#   based on when the flowmeter was working.

library(tidyverse)
library(googlesheets)
library(ggridges)
library(lubridate)

zoop_tows_ws <- gs_title("zoop_tows")
zoop_tows <- gs_read(zoop_tows_ws, ws = "zoop_tows")

DB_WIP <- gs_title("Database WIP")
survey_data <- gs_read(DB_WIP, ws = "survey_data")

survey_zoop <- full_join(zoop_tows, survey_data, "survey_id")

survey_zoop <- survey_zoop %>% 
  mutate(tow_in_secs = period_to_seconds(hms(tow_duration))) %>% 
  mutate(spins = revolutions / tow_in_secs) 

# Here I create a training data set where I am confident the flowmeter was
#   functioning properly.
training_data <- survey_zoop %>%
  filter(survey_date >= "2015-06-04") %>% 
  filter(survey_date <= "2015-07-04") %>%
  filter(region == "DI") %>% 
  select(secchi, tow_duration, volume, spins) %>% 
  na.omit()

# Using the training data set I look at the mean velocity of the flowmeter and
#   calculate the lower bound of 3 standard deviations to use as a cut-off for
#   determining whether a flowmeter was not working properly 
mean <- mean(training_data$spins)
sd <- sd(training_data$spins)
lower_limit <- mean - 3*sd

# To be able to correct tows that are outside 3 standard deviations of rotor
#   velocity, I model the expected volume filtered as a function of tow duration
#   and secchi (to account for net clogging with increased POM).

model <- lm(volume ~ tow_duration + secchi, data = training_data)
model

(model)
summary(model)
plot(model)


# Select tows where the ball bearing is affecting spinning and then find the
# outliers for spins per second to determine the affected tows

wrong_volumes <- survey_zoop %>% 
  filter(survey_date < "2015-06-04") %>% 
  filter(region == "DI") %>% 
  select(tow_duration, secchi, volume, spins, sample_id) %>% 
  filter(spins <= lower_limit) %>% 
  na.omit() %>% 
  mutate(modelled_volume = 0.07706 * tow_duration + 0.4404 * secchi - 4.54557)

modelled_volumes <- wrong_volumes %>% 
  select(sample_id, modelled_volume)

corrected_zoop_tows <- full_join(modelled_volumes, zoop_tows, by = "sample_id")

# After writing out this csv I copy and paste the modelled volumes into the
#   spreadsheet.

write_csv(corrected_zoop_tows,
  "Zooplankton/zoop tow model correction/2017-10-11_corrected_zoop_tows.csv")



