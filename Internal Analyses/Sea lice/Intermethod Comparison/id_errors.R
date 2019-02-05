library('tidyverse')
library('lubridate')
library('googlesheets')
library('ggplot2')

sealice <- gs_title("2017-02_semsp_sealice_method_comparison")

cope <- gs_read(sealice, ws = "cope")
chal <- gs_read(sealice, ws = "chal")
cal_mot <- gs_read(sealice, ws = "cal_mot")
lep_mot <- gs_read(sealice, ws = "lep_mot")

cal_cope <- cope[c(1, 2, 3, 4, 5)] %>%
  filter(result_cal_cope != "None Found")
lep_cope <- cope[c(1, 6, 7, 8, 9)] %>%
  filter(result_lep_cope != "None Found")
chal_a <- chal[c(1, 2, 3, 4, 5)] %>%
  filter(result_chal_a != "None Found")
chal_b <- chal[c(1, 6, 7, 8, 9)] %>%
  filter(result_chal_b != "None Found")
unid_chal <- chal[c(1, 10, 11, 12, 13)] %>%
  filter(result_unid_chal != "None Found")
cm <- cal_mot[c(1, 2, 3, 4, 5)] %>%
  filter(result_cal_mot != "None Found")
cgf <- cal_mot[c(1, 6, 7, 8, 9)] %>%
  filter(result_cgf != "None Found")
lpam <- lep_mot[c(1, 2, 3, 4, 5)] %>%
  filter(result_lpam != "None Found")
lpaf <- lep_mot[c(1, 6, 7, 8, 9)] %>%
  filter(result_lpaf != "None Found")
lam <- lep_mot[c(1, 10, 11, 12, 13)] %>%
  filter(result_lam != "None Found")
laf <- lep_mot[c(1, 14, 15, 16, 17)] %>%
  filter(result_laf != "None Found")
lgf <- lep_mot[c(1, 18, 19, 20, 21)] %>%
  filter(result_lgf != "None Found")

error_rate <- data.frame(
  "class" = c(
    "cal_cope",
    "lep_cope",
    "chal_a",
    "chal_b",
    "unid_chal",
    "cmot",
    "cgf",
    "lpaf"
    )) %>% 
  mutate("n" = c(
    nrow(cal_cope),
    nrow(lep_cope),
    nrow(chal_a),
    nrow(chal_b),
    nrow(unid_chal),
    nrow(cm),
    nrow(cgf),
    nrow(lpaf)
  )) %>% 
  mutate("% L>F" = c(
    length(which(cal_cope$result_cal_cope == "L>F"))/nrow(cal_cope),
    length(which(lep_cope$result_lep_cope == "L>F"))/nrow(lep_cope),
    length(which(chal_a$result_chal_a == "L>F"))/nrow(chal_a),
    length(which(chal_b$result_chal_b == "L>F"))/nrow(chal_b),
    length(which(unid_chal$result_unid_chal == "L>F"))/nrow(unid_chal),
    length(which(cm$result_cal_mot == "L>F"))/nrow(cm),
    length(which(cgf$result_cgf == "L>F"))/nrow(cgf),
    length(which(lpaf$result_lpaf == "L>F"))/nrow(lpaf))) %>% 
  mutate("% F>L" = c(
    length(which(cal_cope$result_cal_cope == "F>L"))/nrow(cal_cope),
    length(which(lep_cope$result_lep_cope == "F>L"))/nrow(lep_cope),
    length(which(chal_a$result_chal_a == "F>L"))/nrow(chal_a),
    length(which(chal_b$result_chal_b == "F>L"))/nrow(chal_b),
    length(which(unid_chal$result_unid_chal == "F>L"))/nrow(unid_chal),
    length(which(cm$result_cal_mot == "F>L"))/nrow(cm),
    length(which(cgf$result_cgf == "F>L"))/nrow(cgf),
    length(which(lpaf$result_lpaf == "F>L"))/nrow(lpaf))) %>% 
  mutate("% Correct" = c(
    length(which(cal_cope$result_cal_cope == "Correct"))/nrow(cal_cope),
    length(which(lep_cope$result_lep_cope == "Correct"))/nrow(lep_cope),
    length(which(chal_a$result_chal_a == "Correct"))/nrow(chal_a),
    length(which(chal_b$result_chal_b == "Correct"))/nrow(chal_b),
    length(which(unid_chal$result_unid_chal == "Correct"))/nrow(unid_chal),
    length(which(cm$result_cal_mot == "Correct"))/nrow(cm),
    length(which(cgf$result_cgf == "Correct"))/nrow(cgf),
    length(which(lpaf$result_lpaf == "Correct"))/nrow(lpaf)))

save(error_rate, file="error_rate.RData")

## This code is complete and good to run again!