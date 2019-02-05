library(hakaisalmon)
library(tidyverse)
library(lubridate)
library(googlesheets)
library(here)

sockeye_2017 <- survey_seines_fish %>% 
  filter(
    species=="SO", 
    survey_date >= as.Date("2017-05-01"),
    analysis_planned=="SEMSP",
    str_detect(ufn, "U")) %>% 
  select(-semsp_id)
  

sample_inventory <- gs_key('1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs', visibility="private", lookup = FALSE)
finclip_inventory <- gs_read(sample_inventory, ws='finclip_metadata')

finclips_sockeye_2017 <- left_join(sockeye_2017,finclip_inventory,by="ufn") %>% 
  select(
    ufn,
    species,
    seine_id,
    survey_id,
    survey_date,
    site_id,
    region,
    zone,
    site_name,
    container_id,
    container_cell,
    sample_location,
    sample_status,
    comments_sample
  )

write.csv(finclips_sockeye_2017, file="2017_sockeye_finclips.csv")