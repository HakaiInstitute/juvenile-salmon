# This is the pre-processing file for the salmon reporting dashboard that will be
# run weekly to update figures throughout the migration period

library(tidyverse)
library(googlesheets)
library(forcats)
library(lubridate)

#####
# Read in field data from 2017
#####
field_2017_workbook <- gs_title("field_data_2017")
field_2017 <- gs_read(field_2017_workbook,ws = "field_data_2017")
field_2017 <- field_2017 [-1, ]  # This removes the metadata row that exists in
        # the google spreadsheet which causes problems with assignment of correct
        # variable classes
write_csv(field_2017, "../raw_data/2017_semsp_field_data.csv")

# writing the data file as a .csv and then reading it again without the metadata
# row facilitates correct variable type assignment

field_2017 <- read_csv("../raw_data/2017_semsp_field_data.csv") 


# Assign sampling week column

field_2017$sampling.week <- NA

field_2017$sampling.week[field_2017$date >= as.Date("2017-05-03") & field_2017$date <= as.Date("2017-05-08")] <- 1
field_2017$sampling.week[field_2017$date >= as.Date("2017-05-09") & field_2017$date <= as.Date("2017-05-15")] <- 2
field_2017$sampling.week[field_2017$date >= as.Date("2017-05-16") & field_2017$date <= as.Date("2017-05-22")] <- 3
field_2017$sampling.week[field_2017$date >= as.Date("2017-05-23") & field_2017$date <= as.Date("2017-06-29")] <- 4
field_2017$sampling.week[field_2017$date >= as.Date("2017-05-30") & field_2017$date <= as.Date("2017-06-05")] <- 5
field_2017$sampling.week[field_2017$date >= as.Date("2017-06-06") & field_2017$date <= as.Date("2017-06-12")] <- 6
field_2017$sampling.week[field_2017$date >= as.Date("2017-06-13") & field_2017$date <= as.Date("2017-06-19")] <- 7
field_2017$sampling.week[field_2017$date >= as.Date("2017-06-20") & field_2017$date <= as.Date("2017-06-26")] <- 8
field_2017$sampling.week[field_2017$date >= as.Date("2017-06-27") & field_2017$date <= as.Date("2017-07-03")] <- 9
field_2017$sampling.week[field_2017$date >= as.Date("2017-07-04") & field_2017$date <= as.Date("2017-07-10")] <- 10
field_2017$sampling.week[field_2017$date >= as.Date("2017-07-11") & field_2017$date <= as.Date("2017-07-18")] <- 11

field_2017$sampling.week <- as.factor(field_2017$sampling.week)

write_csv(field_2017, "../processed_data/field_2017.csv")

#######
# Read in Sea Lice Data
#######
sea_lice_2017 <- gs_read(field_2017_workbook,ws = "sea_lice")
sea_lice_2017 <- sea_lice_2017[-1,]
write_csv(sea_lice_2017, "../processed_data/processed_sea_lice_data.csv")
sea_lice_2017 <- read_csv("../processed_data/processed_sea_lice_data.csv")

sea_lice_2017$sampling.week <-NA

sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-05-03") & sea_lice_2017$date <= as.Date("2017-05-08")] <- 1
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-05-09") & sea_lice_2017$date <= as.Date("2017-05-15")] <- 2
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-05-16") & sea_lice_2017$date <= as.Date("2017-05-22")] <- 3
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-05-23") & sea_lice_2017$date <= as.Date("2017-06-29")] <- 4
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-05-30") & sea_lice_2017$date <= as.Date("2017-06-05")] <- 5
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-06-06") & sea_lice_2017$date <= as.Date("2017-06-12")] <- 6
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-06-13") & sea_lice_2017$date <= as.Date("2017-06-19")] <- 7
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-06-20") & sea_lice_2017$date <= as.Date("2017-06-26")] <- 8
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-06-27") & sea_lice_2017$date <= as.Date("2017-07-03")] <- 9
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-07-04") & sea_lice_2017$date <= as.Date("2017-07-10")] <- 10
sea_lice_2017$sampling.week[sea_lice_2017$date >= as.Date("2017-07-11") & sea_lice_2017$date <= as.Date("2017-07-18")] <- 11

write_csv(sea_lice_2017, "../processed_data/sea_lice_2017.csv")
######
# Read in YSI temperature data
######

ysi_data <- read.csv("../raw_data/ysi_2017.csv")
ysi_data <- as.tbl(ysi_data) %>% 
        rename(temp = Temperature..C.)
ysi_data$Date <- as.Date(ysi_data$Date)

write_csv(ysi_data, "../processed_data/ysi_data.csv")
######
# Read in Chl_a Data
######

# Each week I need to download from the Data Portal the most up to date version of the chl a data and put in the raw data folder

chl_a_data <- read.csv("../raw_data/chlorophyll_2017_07_27.csv", stringsAsFactors = FALSE)
chl_a_data <- as.tbl(chl_a_data)
chl_a_data$Date <- as.Date(chl_a_data$Date)

write_csv(chl_a_data, "../processed_data/chl_a_data.csv")

#####
# Read in Zooplankton Biomass Data
#####

#####
# Read in CTD files
#####

js_ctd <- read.csv("../raw_data/ctd/js_ctd_2017_07_27.csv", stringsAsFactors = FALSE)
js_ctd <- as.tbl(js_ctd)

js_ctd <- separate(js_ctd[-1],Filename,c("unit.ID", "date","time"), "_", extra = "merge")
js_ctd$date<- ymd(js_ctd$date)
js_ctd$region <- "JS"
js_ctd <- js_ctd %>% 
        filter(date > "2017-03-01")
write_csv(js_ctd, "../processed_data/js_ctd.csv")

di_ctd <- read.csv("../raw_data/ctd/di_ctd_2017_07_27.csv", stringsAsFactors = FALSE)
di_ctd <- as.tbl(di_ctd)
di_ctd <- separate(di_ctd[-1],Filename,c("unit.ID", "date","time"), "_", extra = "merge")
di_ctd$date<- ymd(di_ctd$date)
di_ctd$region <- "DI"
di_ctd <- di_ctd %>% 
        filter(date > "2017-03-01")
write_csv(di_ctd, "../processed_data/di_ctd.csv")

