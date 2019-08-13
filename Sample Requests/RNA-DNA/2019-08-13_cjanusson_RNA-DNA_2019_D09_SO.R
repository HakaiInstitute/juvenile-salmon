library(googlesheets)
library(lubridate)
library(tidyverse)
library(dplyr)

labdata <- gs_key("144T4uYN55sY4FHRt7h6QzvvAVrgXc6biFnbxZd_g4z4", visibility = "private", lookup = FALSE)
fish_lab <- gs_read(labdata, ws = "fish_lab_data")

fielddata <- gs_key("1cHgZszv--FlV207cwSpe9hFJipb7HhS4IW9vVrUNg8M", visibility = "private", lookup = FALSE)
fish_field <- gs_read(fielddata, ws = "fish_field_data") %>% 
  select(ufn, seine_id)

so_d09 <- fish_lab %>% 
  filter(species == "SO" & site_id == "D09") %>% 
  left_join(fish_field)

d09_seines <- so_d09 %>%
  group_by(seine_id) %>% 
  summarize(count=n())

so_d09_subset <- so_d09 %>% 
  group_by(seine_id) %>% 
  sample_n(pmin(n(),n)) %>% 
  distinct(ufn, .keep_all = TRUE)

write.csv(so_d09_subset, "2019-08-13_cjanusson_RNA-DNA_2019_D09_SO.csv")
