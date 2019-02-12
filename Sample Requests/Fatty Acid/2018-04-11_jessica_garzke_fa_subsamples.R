library(tidyverse)
library(dplyr)
library(googlesheets)

inv <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", visibility = "private", lookup = FALSE)

containers <- gs_read(inv, ws = "sample_container_inventory")
fa <- gs_read(inv, ws = "fa_metadata")

fa_containers <- left_join(fa, containers, by = "container_id")

jg_wb <- gs_key("1qKDyOSMBXUJw4H6vlD2_gxn_xhmR5WXoCbxuCw-oIn8", visibility = "private", lookup = FALSE)
jg_request <- gs_read(jg_wb, ws = "Fatty Acid Samples")

subsamples <- left_join(jg_request, fa_containers, by = "ufn")

write.csv(subsamples, here::here("2018-09-04_jessica_garzke_fa_sample_locations.csv"))

gs_edit_cells(jg_wb, ws = "sample_locations", anchor = "A1", input = subsamples, byrow = FALSE)
