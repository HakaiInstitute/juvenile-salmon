# This is the script used to generate plots for the Salish Sea Marine Survival
# Program conference on Noember 27th and 28th in Nanaimo BC.

# Setup
library(tidyverse)
library(lubridate)
library(ggridges)
library(forcats)

source("~/Google Drive/SEMSP/SEMSP Data/Analyses/Public Analyses/Data Import Scripts/read_in_googlesheets.R")

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
theme_set(theme_classic())


# Migration Timing


cpue <-  select(survey_seines, survey_date, sampling_week, site_id, region, so_total) %>%
  filter(site_id %in% c("D07", "D09", "D22", "D27", "D10", "D08", "D34",
                        "D20", "J03", "J02", "J09", "J11")) %>%
  # I only included the above sites, because they have been consistent
  # over the last few years.
  mutate(year = year(survey_date)) %>%
  select(-site_id, -survey_date) %>% 
  replace_na(list(so_total = 0)) %>% 
  group_by(year, region, sampling_week) %>% 
  summarise_each(funs(mean, sd, cpue.se=sd(.) / sqrt(n())))
  

cpue$year <- as.factor(cpue$year)
labels <- c(DI = "Discovery Islands", JS = "Johnstone Strait")
cpue$sampling_week <- as_factor(cpue$sampling_week)
dodge <- position_dodge(width=0.5)

cpue_plot <- ggplot(data = cpue, aes(x = sampling_week, y = mean,
                                     colour = year, group = year)) +
  facet_wrap(~ region, nrow = 2, scales = "free_y",
             labeller = labeller(region = labels)) +
  theme(strip.text.x = element_text(size=12)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, position = position_dodge(width = 0.15)) +
  xlab("Date") +
  ylab("Sockeye CPUE") +
  theme(legend.justification = c(1, 0), legend.position = c(.8, .7)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "black", size = 12)) +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) 


cpue_plot

ggsave("./Posters/figures/so_cpue.png", height = 4.5, width = 5.5)

# CPUE with error bars

cpue_plot_error_bars <- ggplot(data = cpue, aes(x = sampling_week, y = mean,
                                     colour = year, group = year)) +
  geom_errorbar(aes(ymin = mean - cpue.se, ymax = mean + cpue.se),
  position = position_dodge(width = 0.15),
  width = 0.3) +
  facet_wrap(~ region, nrow = 2, scales = "free_y",
             labeller = labeller(region = labels)) +
  theme(strip.text.x = element_text(size=12)) +
  geom_line(size = 1.5) +
  geom_point(size = 2, position = position_dodge(width = 0.15)) +
  xlab("Date") +
  ylab("Sockeye CPUE") +
  theme(legend.justification = c(1, 0), legend.position = c(.83, .67)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "black", size = 12)) +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) 


cpue_plot_error_bars

ggsave("./Posters/figures/so_cpue_error_bars.png", height = 4.3, width = 6.3)

# # CPUE as density plot
# 
# survey_seines_fish <- survey_seines_fish %>% 
#   filter(species == "SO") %>% 
#   mutate(julian_date = yday(survey_date)) %>% 
#   mutate(year = as.factor(year(survey_date)))
# ggplot(survey_seines_fish, aes(x = julian_date)) +
#   geom_density(aes(group = year, colour = year, fill = year), alpha = 0.3)

# CPUE as boxplots

cpue_boxplot_tbl <-  select(survey_seines, survey_date, sampling_week, region, zone,
                site_id, so_total) %>%
  filter(site_id %in% c("D07", "D09", "D22", "D27", "D10", "D08", "D34",
                        "D20", "J03", "J02", "J09", "J11")) %>%
  # I only included the above sites, because they have been consistent
  # over the last few years.
  mutate(year = lubridate::year(survey_date)) %>%
  replace_na(list(so_total = 0))

cpue_boxplot <- ggplot(data = cpue_boxplot_tbl, aes(x = sampling_week, y = so_total, fill = as.factor(year)))+
                         geom_boxplot() +
  coord_cartesian(ylim = c(0,1500)) +
  facet_grid(region~sampling_week, scales = "free") +
  theme(strip.text.x = element_blank())
  #facet_wrap(~region, ncol = 1) 

 cpue_boxplot
 ggsave("./Posters/figures/so_cpue_boxplot.png", height = 4.5, width = 5.5)
 
 # GENETIC STOCK ID
contribs_all_years <- survey_seines_fish_stock %>% 
  drop_na(site_id) %>% 
  drop_na(stock_1) %>% 
  mutate(year = as.factor(year(survey_date))) %>% 
  group_by(year, stock_1) %>% 
  summarize(n = n()) %>% 
  mutate(proportion = n / sum(n)) %>% 
  arrange(desc(n)) %>% 
  filter(proportion >= 0.04) %>% 
  ungroup()

prop_remaining <- contribs_all_years %>% 
  group_by(year) %>% 
  summarize(remaining = 1 - sum(proportion))


contribs_all_years_w_others <- contribs_all_years %>% 
  add_row(year = 2015, stock_1 = "Others", proportion = 0.4352641) %>% 
  add_row(year = 2016, stock_1 = "Others", proportion = 0.2404372)

stock_levels <- c(NA, "Chilko", "L_Adams", "Horsefly", "L_Shuswap", "MiddleShuswap",
                  "Mitchell", "Pitt", "Quesnel_Horsef", "Scotch")

contribs_all_years_w_others$stock_1 <- factor(contribs_all_years_w_others$stock_1, 
                                              levels = stock_levels)

big_contribs_plot <- 
  ggplot(contribs_all_years_w_others, aes(x = year,
                                          y = proportion, fill = stock_1)) +
  geom_bar(colour="black", stat="identity", position = 'stack') + 
  scale_fill_discrete(name  ="Genetic Stock",
                      breaks=c("Chilko", "L_Adams", "Horsefly",
                               "L_Shuswap","MiddleShuswap",
                               "Mitchell", "Pitt", "Quesnel_Horsef",
                               "Scotch", NA),
                      labels=c("Chilko", "Lower Adams", "Horsefly",
                               "Lower Shuswap", "Middle Shuswap", "Mitchell", "Pitt", 
                               "Quesnel/Horsefly", "Scotch", "Other")) +
  ylab("Proportion") +
  xlab("Year")+
  theme(legend.text = element_text(colour = "black", size = 12)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold")) 



big_contribs_plot
ggsave("./Posters/Figures/GSI_prop.png", width = 5.5, height = 4.5)

# CONDITION FACTOR

survey_seines_fish$standard_length <- as.numeric(survey_seines_fish$standard_length)

# Subset a test data set for creating a linear model for fork length as a function of 
# standard length. Necessary because caudal fin was often broken during freezing.
modelled_so_fl_tbl <- survey_seines_fish %>% 
  filter(species == "SO") %>% 
  drop_na(fork_length) %>% 
  drop_na(standard_length) %>% 
  select(standard_length, fork_length)

lm <- lm(fork_length ~ standard_length , data = modelled_so_fl_tbl)

# Because not all 2017 fish have been processed yet, I need to include the weight 
# we measured in the field for interannual comparisons (this may be dangerous if fish lose weight after
# being frozen. May have to apply correction to frozen samples.
# Also I apply here a function for modelling fork lengths when that measure wasn't possible
survey_seines_fish <- survey_seines_fish %>% 
  mutate_cond(is.na(weight), weight = as.numeric(weight_field)) %>% 
  mutate_cond(is.na(fork_length), fork_length = as.numeric(fork_length_field)) %>% 
  mutate_cond(is.na(fork_length), fork_length = coef(lm)[[2]] * standard_length + coef(lm)[[1]]) %>%  
  mutate(k = 10^5*(weight / fork_length ^ 3)) 

survey_seines_fish %>%
  filter(species == "SO") %>% 
  mutate(year = as.factor(year(survey_date))) %>% 
  ggplot(aes(x = k)) +
  geom_density(aes(group = year, colour = year, fill = year), alpha = 0.5) +
  facet_wrap(~region, ncol = 1, labeller = labeller(region = labels)) +
  theme(strip.text.x = element_text(size=12)) +
  xlim(0.5:1.5) +
  xlab("Condition Factor (K)") +
  ylab("Density") +
  theme(legend.text = element_text(colour = "black", size = 12)) +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) +
  theme(legend.justification = c(1, 0), legend.position = c(.9, .7)) +
  theme(legend.title = element_blank())


ggsave("./Posters/Figures/so_K.png", width = 5.5, height = 4.5)

# MOTILE SEA LICE INTENSITY

# Assign a categorical sampling year variable to each survey event so that you 
# can group sampling events within a year, to compare between years.

# Below are joins that create views of the data to work with directly

fish_lice <- full_join(survey_seines_fish, sealice_lab, by = "ufn")

# Tidy the data by selecting only the fish that received lab enumeration or that
# were part of Sean Godwin's 2015 subsample (enumerated by Lauren Portner). Sum
# the total counts of both species of sea lice (Caligus clemensi and
# Lepeophtheirus salmonis) per fish, and the total count of all sea lice. Since
# SG's subsample only enumerated fish that were confirmed to have lice, this
# criteria must be applied to all fish in this dataset.
fish_lice_tidy <- fish_lice %>% 
  mutate(sampling_year = as.factor(year(survey_date))) %>% 
  filter(licing_protocol_lab %in% c("lportner","lab_motiles")) %>% 
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
ggplot(weekly_lice, aes(x = sampling_week, y = intensity)) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_line(mapping = aes(color = species, group = species)) +
  geom_errorbar(mapping = aes(ymin = intensity - se, ymax = intensity + se,
                              width = 0.1, color = species)) + 
  facet_grid(sampling_year ~ region, labeller = labeller(region = labels)) +
  theme(strip.text.x = element_text(size=12))+
  theme(strip.text.y = element_text(size=12))+
  xlab("Date") +
  ylab("Mean Motile Infection Intensity")+
  theme(legend.justification = c(1, 0), legend.position = c(.5, .3)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(colour = "black", size = 12, face = "italic")) +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title = element_text(size = 12, face = "bold")) +
  theme(legend.title=element_blank()) +
  scale_colour_discrete(
    breaks=c("cal", "lep"),
    labels=c("Caligus clemensi", "Lepeoptheirus salmonis")) +
  scale_shape_discrete(
    breaks=c("cal", "lep"),
    labels=c("Caligus clemensi", "Lepeoptheirus salmonis")) 

ggsave("./Posters/Figures/motlie_lice_intensity.png", width = 5.5, height = 4.5)


