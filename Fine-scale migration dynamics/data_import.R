library(hakaisalmon)
library(tidyverse)
library(lubridate)
library(here)
library(hakaiApi)
library(DescTools)
library(car)

client <- hakaiApi::Client$new()

# Download stock ID dna (ethanol liver samples) results from EIMS Data Portal
dna_endpoint <- sprintf("%s/%s", client$api_root, 'eims/views/output/jsp_dna?site_id%26%26{"D09"}&limit=-1')
ethanol_dna <- client$get(dna_endpoint)

# Download stock ID finclip (whatman adhered caudal fins) results
finclip_endpoint <-  sprintf("%s/%s", client$api_root, 
                             'eims/views/output/jsp_fin_clip?site_id%26%26{"D09"}&limit=-1')
finclip_dna <- client$get(finclip_endpoint)

# Bind both data sets together
dna <- bind_rows(ethanol_dna, finclip_dna) %>% 
  select(fish_id, stock_1, prob_1) %>% 
  mutate(stock_1 = toupper(stock_1))

# Download seines and Fish Data to join to DNA results

seines <- sprintf("%s/%s", client$api_root, 'eims/views/output/jsp_seine?date>=2015-05-01&date<2016-07-31&site_id~~*"D09"&limit=-1') %>% 
  client$get(.) 

write_csv(seines, here("processed_data", "seines.csv"))

seines_selected <- seines %>% 
  select(seine_id, lat = gather_lat, lon = gather_long)

fish <- sprintf("%s/%s", client$api_root,
                'eims/views/output/jsp_fish?site_id%26%26{"D09"}&limit=-1') %>% client$get(.) %>%
  select(survey_date = date, seine_id, site_id, species, fork_length, weight, ufn = hakai_id) %>%
  left_join(seines_selected) %>% 
  left_join(dna, by = c("ufn" = "fish_id")) %>% 
  # Remove 'nearshore' seines that were actually overlapping with mid-channel sites:
  dplyr::filter(!seine_id %in% c("DE231N2", "DE151N1", "DE301N1", "DE290N2", "DE236N1",
                                 "DE361N2", "DE220N1")) %>% 
  add_column(Habitat = "Nearshore", .after = "ufn") %>% 
  add_column(DNA = NA, .after = "weight") %>% 
  add_column(year_tow = NA, .before = "survey_date") %>% 
  mutate(stock_1 = toupper(stock_1)) %>% 
  mutate(collapsed_fraser_stocks = lump_fraser_sockeye_stocks(stock_1)) %>% 
  select(year_tow, survey_date, site_id, lat, lon, species, fork_length, weight, DNA, stock_1, prob_1, Habitat, seine_id)

# Read in DFO data from D09
dfo_data <- read_csv(here("data", "dfo_data.csv")) %>% 
  add_column(ufn = "ufn", .before = "Habitat", prob_1 = "prob_1") %>% 
  # dplyr::filter out sits that arent from the same DFO sampling station
  dplyr::filter(!year_tow %in% c("2016_109", "2015_78", "2016_34", "2015_5", "2015_73",
                                 "2015_9", "2015_37", "2015_20", "2016-106")) %>% 
  mutate(prob_1 = as.numeric(prob_1),
         survey_date = as_date(survey_date),
         seine_id = NA) 

# Join Hakai and DFO data into one dataframe
okisollo <- bind_rows(fish, dfo_data) %>%
  mutate(stock_1 = toupper(stock_1)) %>% 
  mutate(collapsed_fraser_stocks = lump_fraser_sockeye_stocks(stock_1)) %>% 
  mutate(k = 10^5*(weight / fork_length ^ 3)) %>% 
  mutate(year = year(survey_date)) %>% 
  dplyr::filter(year %in% c(2016, 2015)) %>% 
  # categorize sampling events into categorical week and recod the categories so
  # that the middle of the week is the label
  mutate(sampling_week = as.numeric((yday(survey_date) + 4) %/% 7)) %>%
  mutate(
    sampling_week = recode_factor(sampling_week,
                                  `18` = "May 5",
                                  `19` = "May 12" ,
                                  `20` = "May 19",
                                  `21` = "May 26",
                                  `22` = "June 2",
                                  `23` = "June 9",
                                  `24` = "June 16",
                                  `25` = "June 23",
                                  `26` = "June 30",
                                  `27` = "July 6",
                                  `28` = "July 13"
    )
  ) %>% 
  #Looks like dfo data has errors in it with a fork_length of 9 and 18
  dplyr::filter(fork_length > 20)

write_csv(okisollo, here("processed_data", "okisollo.csv"))
  
# Download sampling coordinates
coords <- googlesheets::gs_key("157froMwaYUlXTCkGRBrFcQzTlrZEJZmWn89P-xAO1v0", visibility = "private", lookup = FALSE)

dfo_coords <- googlesheets::gs_read(coords, ws = "dfo") %>% 
  mutate(org = "Mid Channel",
         seine_id = NA) %>% 
  dplyr::select(seine_id, lat, lon, org) %>% 
  dplyr::filter(lat > 50.3, lon < -125.315) 

write_csv(dfo_coords, here("processed_data", "dfo_coords"))

  dplyr::filter(fork_length > 20)