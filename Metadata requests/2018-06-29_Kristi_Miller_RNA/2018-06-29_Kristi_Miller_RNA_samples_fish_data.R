library(hakaisalmon)
library(tidyverse)

# This selection of metadata is to accompany the RNA samples that were processed
# by Carly Janusson and sent to Kristi Millers lab at the Pacific Biological
# Station in June 2018

#Create table of the tray sets that Carly sent to PBS
trays <- as_tibble(c("BRN15",
                     "BRN16",
                     "BRN26",
                     "BRN40",
                     "BRN41",
                     "BRN42",
                     "BRN43",
                     "BRN44",
                     "BRN20",
                     "BRN23",
                     "BRN35",
                     "BRN38")) %>% 
  rename("container_id" = "value")

# Join sample location metadata 
rna <- left_join(trays, rna_metadata, by = "container_id")

# Join sample data to survey, seines, and fish metadata
fish_sent <- left_join(rna, survey_seines_fish_stock_id, by = "ufn") %>% 
  select(sample_location,ufn, species, survey_date, site_id, lat, lon, standard_length,
         fork_length, weight, date_processed,
         stock_1, prob_1, stock_2, prob_2, stock_3, prob_3,
         container_id,
         comments_sample, comments_fish_health_lab, comments_protocol) %>% 
  write_csv(here::here("Metadata requests", "2018-06-29_Kristi_Miller_RNA", "Hakai_Kristi_MIller_RNA_samples_with_fish_data.csv"))
