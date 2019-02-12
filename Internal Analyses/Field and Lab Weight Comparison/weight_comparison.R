library(googlesheets)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(ggplot2)

fish_2017 <- read_excel("Field and Lab Weight Comparison/2017_fish.xlsx", sheet = "Hakai Data") %>% 
  filter(!is.na(`Weight Lab (g)`)) %>% 
  select(`Hakai ID`, `Species`, `Date Caught` = `Date`, `Site ID`, `Date Processed`, `Fork Length Field (mm)`, `Fork Length Lab (mm)`, weight_wet = `Weight Field (g)`, weight_freezer = `Weight Lab (g)`)

fish_2018_field <- read_excel("Field and Lab Weight Comparison/2018_fish.xlsx", sheet = "Hakai Data") %>% 
  select(`Hakai ID`, `Species`, `Date Caught` = `Date`, `Site ID`, `Fork Length Field (mm)`, weight_wet = `Weight Field (g)`) %>% 
  filter(!is.na(weight_wet))
fish_2018_lab <- read_excel("Field and Lab Weight Comparison/2018-19_semsp_lab_data.xlsx", sheet = "fish_lab_data") %>% 
  select(`Hakai ID` = `ufn`, weight_freezer = `weight_lab`, `Date Processed` = date_processed, `Fork Length Lab (mm)` = fork_length)
fish_2018 <- left_join(fish_2018_field,fish_2018_lab) %>% 
  filter(!is.na(weight_freezer) & weight_freezer != "NA") %>% 
  mutate(weight_freezer = as.numeric(weight_freezer))

# Calculate the difference and % difference between wet & freezer weights
fish_weights <- rbind(fish_2017,fish_2018) %>% 
  mutate(storage_time = as.numeric(`Date Processed` - `Date Caught`)) %>% 
  mutate(weight_diff = weight_wet - weight_freezer) %>% 
  mutate(weight_diff_percent = (weight_wet - weight_freezer)/weight_freezer*100)

ggplot(fish_weights, aes(x=storage_time, y = weight_diff)) +
  geom_point(shape=1)+
  geom_smooth(method=lm)

# Filter out outliers
fish_weights_no_outliers <-  fish_weights %>% 
 filter(!`Hakai ID` %in% c("U17302", "U16916", "U10246", "U17739", "U17087", "U10247"))

# Create a linear multiple regression model
mod1 <- lm(weight_diff ~ storage_time + weight_freezer, data = fish_weights_no_outliers)
summary(mod1)
plot(mod1)

# Is there a significant difference between field & freezer weights?
t.test(fish_weights$weight_wet,fish_weights$weight_freezer, paired=TRUE, conf.level=0.95)
plot(fish_weights_no_outliers$weight_diff)
hist(fish_weights_no_outliers$weight_diff)
shapiro.test(fish_weights_no_outliers$weight_diff_percent)

# Nonparametric Wilcox test
wilcox.test(fish_weights_no_outliers$weight_wet,fish_weights_no_outliers$weight_freezer, paired=T)

# Make a boxplot
fish_weights_tidy <- fish_weights_no_outliers %>%
  select(`Hakai ID`, `field` = `weight_wet`, `lab` = weight_freezer) %>% 
  gather(`field`, `lab`, key = "weight_state", value = "weight")

ggplot(fish_weights_tidy, aes(x=weight_state,y=weight)) +
  geom_boxplot()

# Calculate means and 95% CIs
fish_weights_tidy %>%
  group_by(weight_state) %>%
  summarise(mean = mean(weight, na.rm = TRUE),
            sd = sd(weight, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
