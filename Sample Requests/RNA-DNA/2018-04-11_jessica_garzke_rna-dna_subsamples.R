library(tidyverse)
library(dplyr)
library(googlesheets)

inv <- gs_key("1Ti5gGvakA4DUTjCUZ_VYHULU_FJCK05-zdly5E80Tzs", visibility = "private", lookup = FALSE)

containers <- gs_read(inv, ws = "sample_container_inventory")
rna_m <- gs_read(inv, ws = "rna-muscle_metadata")

rna_containers <- left_join(rna_m, containers, by = "container_id")

jg_wb <- gs_key("1xivYe-w0YKOPbbnhz1N3l9F-T0L8eT7htiL_aZrD-Bw", visibility = "private", lookup = FALSE)
jg_request <- gs_read(jg_wb, ws = "RNA DNA samples")

subsamples <- left_join(jg_request, rna_containers, by = "ufn")

gs_edit_cells(jg_wb, ws = "sample_locations", anchor = "A1", input = subsamples, byrow = FALSE)
