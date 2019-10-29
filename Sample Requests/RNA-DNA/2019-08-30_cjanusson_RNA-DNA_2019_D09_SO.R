library(googlesheets)
library(lubridate)
library(tidyverse)
library(dplyr)

set.seed(123)

labdata <- gs_key("144T4uYN55sY4FHRt7h6QzvvAVrgXc6biFnbxZd_g4z4", visibility = "private", lookup = FALSE)
fish_lab <- gs_read(labdata, ws = "fish_lab_data")

fielddata <- gs_key("1cHgZszv--FlV207cwSpe9hFJipb7HhS4IW9vVrUNg8M", visibility = "private", lookup = FALSE)
fish_field <- gs_read(fielddata, ws = "fish_field_data") %>% 
  select(ufn, seine_id)

rna_dna <- gs_read(labdata, ws = "rna:dna")

so_d09 <- fish_lab %>% 
  filter(species == "SO" & site_id == "D09") %>% 
  left_join(fish_field)

d09_seines <- so_d09 %>%
  group_by(seine_id) %>% 
  summarize(count=n())

fish_already_sampled <- read_csv("Sample Requests/RNA-DNA/2019-08-13_cjanusson_RNA-DNA_2019_D09_SO.csv")


so_d09_remaining <- so_d09 %>% 
  filter(!ufn %in% fish_already_sampled$ufn) %>% 
  filter(ufn != "U20139" | ufn != "U20044") # These fish need their RNA re-sampled due to cross-contamination

so_d09_subset_2 <- so_d09_remaining %>% 
  group_by(seine_id) %>% 
  sample_n(if(n()<5) n() else 5) %>% 
  left_join(select(rna_dna, ufn, sample_id, sample_comments))

write_csv(so_d09_subset_2, "Sample Requests/RNA-DNA/2019-08-30_cjanusson_RNA-DNA_2019_D09_SO.csv")
so_d09_subset_2 <- read_csv("Sample Requests/RNA-DNA/2019-08-30_cjanusson_RNA-DNA_2019_D09_SO.csv")

elab <-  read_tsv("Sample Requests/RNA-DNA/elab_2019-08-30_cjanusson_RNA-DNA_2019_D09_SO.csv")

so_d09_subset_loc <- left_join(so_d09_subset_2, elab, by = c("sample_id" = "barcode")) %>% 
  select(1:5,23,31:33)
write_csv(so_d09_subset_loc, "Sample Requests/RNA-DNA/2019-08-30_cjanusson_RNA-DNA_2019_D09_SO.csv")
