library('tidyverse')
library('lubridate')
library('ggplot2')

fish_field_data <- read_csv("Data Packages/Current/fish_field_data.csv")
fish_lab_data <- read_csv("Data Packages/Current/fish_lab_data.csv")
sealice_lab_motiles <- read_csv("Data Packages/Current/sealice_lab_motiles.csv")
seine_data <- read_csv("Data Packages/Current/seine_data.csv")
survey_data <- read_csv("Data Packages/Current/survey_data.csv")


# Assign a categorical sampling year variable to each survey event so that you 
# can group sampling events within a year, to compare between years.

survey_data <- survey_data %>%
  mutate(sampling_year = as.factor(year(survey_date)))

# Below are joins that create views of the data to work with directly
survey_seines <- full_join(survey_data, seine_data, by = "survey_id")
fish_data <- full_join(fish_field_data, fish_lab_data)
survey_seines_fish <- full_join(survey_seines, fish_data, by = "seine_id")
fish_lice <- full_join(survey_seines_fish, sealice_lab_motiles, by = "ufn")

# Tidy the data by selecting only the fish that received lab enumeration or that
# were part of Sean Godwin's 2015 subsample (enumerated by Lauren Portner). Sum
# the total counts of both species of sea lice (Caligus clemensi and
# Lepeophtheirus salmonis) per fish, and the total count of all sea lice. Since
# SG's subsample only enumerated fish that were confirmed to have lice, this
# criteria must be applied to all fish in this dataset.
fish_lice_tidy <- fish_lice %>% 
  filter(lice_id_protocol_lab %in% c("lportner","lab_motiles")) %>% 
  mutate(total_cal = cm_lab + cpaf_lab + caf_lab + cgf_lab + ucal_lab) %>% 
  mutate(total_lep = lgf_lab + laf_lab + lpaf_lab + lpam_lab + lam_lab + 
           ulep_lab) %>% 
  mutate(total_motiles = total_cal + total_lep) %>% 
  filter(total_motiles > 0) %>% 
  select(region, sampling_week, sampling_year, species, total_cal, total_lep, 
         total_motiles)
  

# Filter for sockeye, since they were the only species that received lice 
# enumeration in 2015,
so_lice <- filter(fish_lice_tidy, species == "SO")

# Calculate the weekly average infection intensity of Cal & Lep motiles on
# infected sockeye
weekly_lice <- so_lice %>%
  group_by(region, sampling_week, sampling_year) %>%
  summarize(
    cal = mean(total_cal, na.rm = T),
    lep = mean(total_lep, na.rm = T)) %>% 
  gather(cal, lep, key = "species", value = "intensity")

weekly_lice_se <- so_lice %>% 
  group_by(region, sampling_week, sampling_year) %>% 
  summarize(
    cal = sd(total_cal, na.rm = T)/sqrt(n()),
    lep = sd(total_lep, na.rm = T)/sqrt(n())) %>%
  gather(cal, lep, key = species, value = "se")
  
weekly_lice$se <- weekly_lice_se$se %>% 
  replace(is.na(weekly_lice_se$se), 0)

weekly_lice$sampling_week <- as.factor(weekly_lice$sampling_week)


# Plot the data
ggplot(weekly_lice, aes(x = sampling_week, y = intensity, group = region)) +
  geom_point(mapping = aes(color = sampling_year, shape = sampling_year)) +
  geom_line(mapping = aes(color = sampling_year, group = sampling_year)) +
  geom_errorbar(mapping = aes(ymin = intensity - se, ymax = intensity + se,
    width = 0.1, color = sampling_year)) + 
  facet_grid(region ~ species) +
  xlab("Date") +
  ylab("Mean Motile Infection Intensity") +
  scale_x_discrete(labels = c("May 12", "May 19", "May 26", "June 2", "June 9", 
                       "June 16", "June 23", "June 30", "July 6", "July 14"))
  

