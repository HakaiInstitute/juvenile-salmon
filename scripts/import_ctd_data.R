## Get CTD data from EIMS database using R API
library(tidyverse)
client <- hakaiApi::Client$new()
di_sockeye_endpoint = sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?cruise=DISOCKEYE&limit=-1")
js_sockeye_endpoint = sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?cruise=JSSOCKEYE&limit=-1")
di_ctd_data <- client$get(di_sockeye_endpoint)
write_csv(di_ctd_data, here::here("In-season Reports", "2018_in_season_report",
                                  "data", "di_ctd_data.csv"))
js_ctd_data <- client$get(js_sockeye_endpoint)
write_csv(js_ctd_data, here::here("In-season Reports", "2018_in_season_report",
                                  "data", "js_ctd_data.csv"))

qu39_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=QU39&limit=-1")
qu39 <- client$get(qu39_endpoint)
write_csv(qu39, here::here("In-season Reports", "2018_in_season_report",
                           "data", "qu39.csv"))

qu29_endpoint <- sprintf("%s/%s", client$api_root, "ctd/views/file/cast/data?station=QU29&limit=-1")
qu29 <- client$get(qu29_endpoint)
write_csv(qu29, here::here("In-season Reports", "2018_in_season_report",
                           "data", "qu29.csv"))