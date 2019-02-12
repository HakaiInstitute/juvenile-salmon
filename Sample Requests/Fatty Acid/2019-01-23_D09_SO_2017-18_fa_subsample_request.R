library(googlesheets)
library(tidyverse)
library(lubridate)
library(here)

inv <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", visibility = "private", lookup = FALSE)
fa <- gs_read(inv, ws = "fa_metadata")
fish <- gs_read(inv, ws = "fish")
boxes <- gs_read(inv, ws = "sample_container_inventory")

fa_fish <- right_join(fish, fa, by = c("Hakai ID" = "ufn"))
fa_fish_2017_18 <- fa_fish %>% 
  filter(Date > as.Date("2017-01-01"))

fa_loc <- left_join(fa_fish_2017_18, boxes, by = "container_id") %>% 
  select(-container_num, -container_grid_size, -(27:33))

write.csv(fa_loc, "JSP_2017-2018_fish_fattyacids.csv")

fa_d09_so <- fa_loc %>% 
  filter(`Site ID` == "D09" & `Species` == "SO")

write.csv(fa_d09_so, "2019-01-23_jessica_garzke_fa_sample_locations.csv")
