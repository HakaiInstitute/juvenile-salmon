library(tidyverse)
library(lubridate)
library(here)

survey_seines <- hakaisalmon::survey_seines %>% 
  # remove ad-hoc collections from migration timing calcs
  filter(survey_type == "standard",
         collection_protocol == "SEMSP",
         set_type == "targeted") %>%
  # only include consistently sampled sites with similar catchabilities
  filter(
    site_id %in% c(
      "D07",
      "D09",
      "D22",
      "D27",
      "D10",
      "D08",
      "D34",
      "D20",
      "J03",
      "J02",
      "J09",
      "J11"
    )
  ) %>%
  mutate(year = year(survey_date),
         yday = yday(survey_date)
  ) %>% 
  # filter out sampling events from beyond the normal sampling period. (May 1 - July 9)
  filter(yday < 190) 

catch_intensity <- survey_seines %>% 
  rename("Sockeye" = "so_total", "Pink" = "pi_total", "Chum" = "cu_total", "Coho" = "co_total") %>%
  # remove seines that did not catch any sockeye, even if they caught other spp
  filter(Sockeye > 0) %>% 
  select(seine_id, survey_id, set_number, year, survey_date, region, zone, site_id, site_name, Sockeye, Pink, Chum, Coho)


write.csv(catch_intensity, "Metadata requests/2019-04-27_Randall_Peterman_jsp_timeseries_catch_statistics/2019-04-27_Randall_Peterman_jsp_timseries_catch_statistics.csv", row.names = FALSE)
