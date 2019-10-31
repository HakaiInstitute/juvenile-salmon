library(tidyverse)
library(googlesheets)
library(here)

lab <- gs_key("144T4uYN55sY4FHRt7h6QzvvAVrgXc6biFnbxZd_g4z4", visibility = "private", lookup = FALSE)
fish <- gs_read(lab, ws = "fish_lab_data") %>% 
  select(ufn, species, survey_date, site_id, dissector) %>% 
  drop_na(ufn)

# Copy the sample IDs from Google Sheets, paste into eLab's Advanced Search function (enable Multiline), and then export the file.
# eLab saves file as tab-delimited with a .csv extension. If importing into R raw, use the `read_tsv()` function.
## If you need to make edits in Excel first, import the file using Data > From Text to specify tab delimited (otherwise it incorrectly reads the csv)
## Save the file as a proper .csv and then use `read_csv()`


# RNA:DNA -----------------------------------------------------------------

rna_dna <- gs_read(lab, ws = "rna:dna") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = paste(container.id, input.row, input.col, sep=".")) %>% 
  select(sample_id, dissector, date_processed, pos_gs)


elab_rna_dna <- read_csv(here("data", "elab_export", "elab_rna_dna.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

rna_dna_qc = full_join(rna_dna, elab_rna_dna, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# RNA Pathogens -----------------------------------------------------------

rna_path <- gs_read(lab, ws = "rna_path") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = paste(container.id, input.row, input.col, sep=".")) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_rna_path <- read_csv(here("data","elab_export","elab_rna_path.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

rna_path_qc = full_join(rna_path, elab_rna_path, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# Fatty Acid --------------------------------------------------------------

fa <- gs_read(lab, ws = "fatty_acid") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = ifelse(is.na(input.col), paste(container.id, `elab.position`, sep="."), paste(container.id, input.col, input.row, sep="."))) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_fa <- read_tsv(here("data","elab_export","elab_fa.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

fa_qc <- full_join(fa, elab_fa, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# Extra Muscle ------------------------------------------------------------

xm <- gs_read(lab, ws = "extra_muscle") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = ifelse(is.na(input.col), paste(container.id, `elab.position`, sep="."), paste(container.id, input.col, input.row, sep="."))) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_xm <- read_tsv(here("data","elab_export","elab_xm.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

xm_qc <- full_join(xm, elab_xm, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# DNA ---------------------------------------------------------------------

dna <- gs_read(lab, ws = "DNA") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = ifelse(is.na(input.col), paste(container.id, `elab.position`, sep="."), paste(container.id, input.col, input.row, sep="."))) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_dna <- read_tsv(here("data", "elab_export", "elab_dna.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

dna_qc <- full_join(dna, elab_dna, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# Scale -------------------------------------------------------------------

scale <- gs_read(lab, ws = "scale") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = paste(container.id, elab.position, sep=".")) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_scale <- read_tsv(here("data", "elab_export", "elab_scale.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

scale_qc <- full_join(scale, elab_scale, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# Sea Lice Microbiome -----------------------------------------------------

sl <- gs_read(lab, ws = "sea_lice") %>% 
  left_join(fish, by = "ufn") %>% 
  mutate(pos_gs = ifelse(is.na(input.col), paste(container.id, `elab.position`, sep="."), paste(container.id, input.col, input.row, sep="."))) %>% 
  select(sample_id, dissector, date_processed, pos_gs)

elab_slmicro <- read_tsv(here("data", "elab_export", "elab_slmicro.csv")) %>% 
  mutate(pos_el = paste(storageLayerName, `Storage Position`, sep=".")) %>% 
  select(sample_id = name, pos_el)

slmicro_qc <- full_join(sl, elab_slmicro, by = "sample_id") %>% 
  mutate(match = ifelse(pos_gs == pos_el, "Y", "N")) %>% 
  filter(match == "N")


# Stomachs ----------------------------------------------------------------

stom_up <- gs_read(lab, ws = "stomach_unprocessed") %>% 
  left_join(fish, by = "ufn") %>% 
  filter(is.na(sample_id_2)) %>% 
  select(sample_id_1, dissector, date_processed, stomach.bag.number, sample_id_2)

elab_stom_up <- read_tsv(here("data", "elab_export", "elab_stom_up.csv")) %>% 
  select(sample_id_1 = name, storageLayerName)

stom_up_qc <- full_join(stom_up, elab_stom_up, by = "sample_id_1") %>% 
  mutate(match = ifelse(stomach.bag.number == storageLayerName, "Y", "N")) %>% 
  filter(match == "N" | is.na(match))