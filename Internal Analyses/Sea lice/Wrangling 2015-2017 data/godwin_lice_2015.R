library(tidyverse)
library(lubridate)
library(googlesheets)

setwd("~/R")

godwin_lice_2015_workbook <- gs_title("2017-10-02_seans_2015_lice_data.xlsx")
godwin_lice_2015 <- gs_read(godwin_lice_2015_workbook,
                            ws = "Lice identification")

godwin_lice_2015 <- godwin_lice_2015 %>%
  gather(`lep.cop`, `lep.cunifer.cop`, `lep.chal.a`, `lep.chal.b`, `lep.pa.m.1`,
         `lep.pa.m.2`, `lep.pa.f.1`, `lep.pa.f.2`, `lep.a.m`, `lep.a.f`, 
         `lep.grav.f`, `cal.cop`, `cal.chal.a.1`, `cal.chal.a.2`, 
         `cal.chal.b.3`, `cal.chal.b.4.f`, `cal.chal.b.4.m`, `cal.chal.4.unid`, 
         `cal.chal.a.unid`, `cal.chal.b.unid`, `cal.pa.m`, `cal.pa.f`, 
         `cal.a.m`, `cal.a.f`, `cal.grav.f`, `cal.mot.unid`, `unidentified`, 
         key = "life_stage", value = "count") %>% 
  select(-c(date.liced, ufn, num.lice.report, num.lice.found, comments))

write_csv(godwin_lice_2015, "godwin_lice_2015.csv")