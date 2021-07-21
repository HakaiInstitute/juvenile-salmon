library(googlesheets4)
library(tidyverse)
library(here)

sealice <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/sample_results/sealice_microbiome.csv")
surveys <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/survey_data.csv")
seines <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/seine_data.csv")
ss <- left_join(seines, surveys, by = "survey_id")
fish <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/fish_field_data.csv", guess_max = 20000)

fish_ss <- left_join(fish, ss, by = "seine_id") %>% 
  select(ufn,
         species,
         seine_id,
         survey_date,
         site_id)

sealice_fish <- left_join(sealice, fish_ss, by = "ufn") %>% 
  select(sample_id, sample_type, ufn, species:site_id, sample_comments)

write_csv(sealice_fish, here::here("Sea Lice", "2019-12-06_Tianyi_Chang_sealice_microbiome.csv"))

sealice_elab <- read_tsv(here::here("Sea Lice", "2019-12-06_Tianyi_Chang_sealice_microbiome_eLab.csv")) %>% 
  select(sample_id = name, container_id = storageLayerName, position = `Storage Position`)

sealice_join <- left_join(sealice_fish, sealice_elab, by = "sample_id")
write_csv(sealice_join, here::here("sea Lice", "2019-12-06_Tianyi_Chang_sealice_microbiome.csv"))
