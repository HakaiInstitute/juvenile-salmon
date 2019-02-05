library(tidyverse)
library(googlesheets)
library(hakaisalmon)
library(lubridate)

vaness <- gs_key("1sCmrDjQFGO4WOATRsTPjGaBgzMVZiU_ea1RZBUP3rws",
       lookup = FALSE, visibility = "private")
vaness_stom <- gs_read(vaness, ws = "Sheet 1")

#download SEMSP Inventory googlesheet
samples<- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs",
                 lookup = FALSE, visibility = "private")

current_stomach_metadata <- gs_read(samples, ws = "stomach_metadata")

current_sample_container_inventory <- gs_read(samples, ws = "sample_container_inventory",
                                              lookup = FALSE, visibility = "private")

                                              
vaness_stom_dissected <- vaness_stom %>% 
  filter(status == "dissected") %>% 
  select(ufn) %>% 
  left_join(current_stomach_metadata) %>% 
  left_join(current_sample_container_inventory) %>% 
  select(ufn, container_id, date_inventoried) %>% 
  arrange(desc(container_id)) %>% 
  write_csv(here::here("Metadata requests", "2018-07-13_Vanessa_stomachs",
                       "2018-07-13_vanessa_PI_CU_stomach_samples.csv"))

## Look to see how many are dissected according to our records


            