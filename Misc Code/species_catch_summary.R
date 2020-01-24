library(tidyverse)
library(here)
library(lubridate)

surveys <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv")
seines <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv")

ss <- left_join(seines, surveys)

ss_2019 <- ss %>% 
  filter(survey_date > as.Date("2019-01-01")) %>% 
  mutate(region = case_when(str_detect(site_id, "D") ~ "Discovery Islands",
                            str_detect(site_id, "J") ~ "Johnstone Strait")) %>% 
  group_by(region)

taken <- ss_2019 %>% 
  summarize("Salmon, Sockeye" = sum(so_taken, na.rm = TRUE),
            "Salmon, Pink" = sum(pi_taken, na.rm = TRUE),
            "Salmon, Chum" = sum(cu_taken, na.rm = TRUE),
            "Salmon, Coho" = sum(co_taken, na.rm = TRUE),
            "Pacific Herring" = sum(he_taken, na.rm = TRUE),
            "Salmon, Chinook" = sum(ck_taken, na.rm = TRUE)) %>% 
  pivot_longer(-region, names_to = "species", values_to = "n_taken") %>% 
  mutate(pk = paste(region, species, sep="-")) %>% 
  mutate(age_class = "J") %>% 
  select(region, species, age_class, n_taken, pk)

total <- ss_2019 %>% 
  summarize("Salmon, Sockeye" = sum(so_total, na.rm = TRUE),
            "Salmon, Pink" = sum(pi_total, na.rm = TRUE),
            "Salmon, Chum" = sum(cu_total, na.rm = TRUE),
            "Salmon, Coho" = sum(co_total, na.rm = TRUE),
            "Pacific Herring" = sum(he_total, na.rm = TRUE),
            "Salmon, Chinook" = sum(ck_total, na.rm = TRUE)) %>% 
  pivot_longer(-region, names_to = "species", values_to = "n_total") %>% 
  mutate(pk = paste(region, species, sep="-"))

bycatch <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/bycatch_mort.csv") %>% 
  left_join(select(ss, seine_id, survey_date)) %>% 
  filter(survey_date > as.Date("2019-01-01")) %>% 
  mutate(region = case_when(str_detect(seine_id, "D") ~ "Discovery Islands",
                            str_detect(seine_id, "J") ~ "Johnstone Strait")) %>% 
  group_by(region, bm_species, bm_ageclass) %>% 
  summarize(n_total = sum(bm_count)) %>% 
  mutate(n_taken = 0) %>% 
  rename(species = bm_species,
         age_class = bm_ageclass) %>% 
  filter(species != "unknown")


catch <- full_join(taken, select(total, pk, n_total)) %>% 
  select(-pk) %>% 
  bind_rows(bycatch) %>% 
  arrange(region, species)

write_csv(catch, here::here("Internal Reports", "2019_JSP_catch_summary.csv"))

          