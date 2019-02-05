library(tidyverse)
library(googlesheets)

setwd("/Users/brett.johnson/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses")
dissect_work_book<- gs_title("2017-10-25_Core_Remaining_2015_PI_CU_CO_HE_D07_D08_J03_D06.csv")
PI_CU_2015_core_dissections_update <- gs_read(dissect_work_book, "2015_PI_CU_CO_HE_D07_D08_J03_D06")
write_csv(PI_CU_2015_core_dissections_update, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/PI_CU_2015_core_dissections_update.csv")

fish_field_data <- read_csv("Data Packages/Current/fish_field_data.csv")
fish_lab_data <- read_csv("Data Packages/Current/fish_lab_data.csv")
sealice_lab_motiles <- read_csv("Data Packages/Current/sealice_lab_motiles.csv")
seine_data <- read_csv("Data Packages/Current/seine_data.csv")
survey_data <- read_csv("Data Packages/Current/survey_data.csv")
sample_metadata <- read_csv("Data Packages/Current/sample_metadata.csv")
sample_container_inventory <- read_csv("Data Packages/Current/sample_container_inventory.csv")
stock_id <- read_csv("Data Packages/Current/stock_id.csv")



write_csv(sample_metadata, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/sample_metadata.csv")
write_csv(sample_container_inventory, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/sample_container_inventory.csv")
write_csv(seine_data, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/seine_data.csv")
write_csv(fish_field_data, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/fish_field_data.csv")
write_csv(fish_lab_data, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/fish_lab_data.csv")
write_csv(sealice_lab_motiles, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/sealice_lab_motiles.csv")
write_csv(survey_data, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/survey_data.csv")
write_csv(stock_id, "~/Google Drive/SEMSP/SEMSP Data/Analyses/Internal Analyses/Sea lice/Sea Lice Strategy/stock_id.csv")
