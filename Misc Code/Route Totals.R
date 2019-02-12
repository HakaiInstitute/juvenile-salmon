library(hakaisalmon)
library(tidyverse)
library(forcats)

survey_seines <- hakaisalmon::survey_seines %>% 
  mutate(zone = ifelse(site_id == "D27", "W", zone))

survey_seines$zone <- factor(survey_seines$zone) %>% 
  fct_relevel("W", "C", "E")

levels(survey_seines$zone)

class(survey_seines$zone)

abundance <- survey_seines %>% 
  select(survey_date, region, zone, site_id, Sockeye = so_total, Pink = pi_total, Chum = cu_total, Coho = co_total, Herring =  he_total,
         Chinook = ck_total) %>%
  group_by(region, zone) %>% 
  summarize_if(is.numeric, sum, na.rm = T) %>% 
  gather(species, value = n, Sockeye:Chinook, -region, -zone) %>% 
  filter(region != "JS", species %in% c("Chum", "Pink", "Sockeye")) %>% 
  drop_na(zone) %>% 
  ggplot(aes(x = zone, y = n, fill = species)) +
  geom_bar(position = position_dodge(0.9), stat = "identity") +
  facet_grid(species ~ .) + 
  scale_fill_hakai() +
  ggtitle("Catch Total")

abundance

ggsave(here::here("figs", "route_totals.png"))

intensity <- survey_seines %>% 
  select(survey_date, region, zone, Sockeye = so_total, Pink = pi_total, Chum = cu_total, Coho = co_total, Herring =  he_total,
         Chinook = ck_total) %>%
  # constrain to seines with > 0 sockeye to calculate intensity
  filter(Sockeye > 0) %>% 
  group_by(region, zone) %>% 
  summarize_if(is.numeric, mean, na.rm = T) %>% 
  gather(species, value = n, Sockeye:Chinook, -region, -zone) %>% 
  filter(region != "JS", species %in% c("Chum", "Pink", "Sockeye")) %>% 
  drop_na(zone) %>% 
  ggplot(aes(x = zone, y = n, fill = species)) +
  geom_bar(position = position_dodge(0.9), stat = "identity") +
  facet_grid(species ~ .) + 
  ggtitle("Catch Intensity") +
  scale_fill_hakai()

intensity

cowplot::plot_grid(abundance, intensity, ncol = 1)
