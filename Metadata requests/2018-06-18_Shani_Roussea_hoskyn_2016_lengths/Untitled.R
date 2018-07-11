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
rna <- left_join(rna_metadata, trays, by = "container_id")

# Join sample data to survey, seines, and fish metadata
fish_sent <- left_join(rna, survey_seines_fish_stock_id, by = "ufn") %>% 
  select(ufn, species, survey_date, site_id, fork_length, weight, 
