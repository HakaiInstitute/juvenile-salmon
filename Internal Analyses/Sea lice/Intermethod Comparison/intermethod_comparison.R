
library('tidyverse')
library('lubridate')
library('googlesheets')
library('ggplot2')
library('coin')

sealice <- gs_title("2017-02_semsp_sealice_method_comparison")

# Compare total counts in lab vs. in field ------------------------------------
all_lab <- gs_read(sealice, ws = "sealice_fine_fieldcat") %>% 
  select(ufn, total_attached_lab, total_motile_lab)

all_field <- gs_read(sealice, ws = "sealice_field") %>% 
  select(ufn, total_attached_field, total_motile_field) 

lab_vs_field <- left_join(all_lab,all_field, by="ufn") %>% 
  mutate(t_attached_lab = sqrt(total_attached_lab),
         t_motile_lab = sqrt(total_motile_lab),
         t_attached_field = sqrt(total_attached_field),
         t_motile_field = sqrt(total_motile_field))

all_lab_tidy <- all_lab %>% 
  rename(`attached` = `total_attached_lab`, `motile` = `total_motile_lab`) %>% 
  gather(`attached`, `motile`, key = "stage", value = "count") %>% 
  mutate(method = paste("lab_fine"))

all_field_tidy <- all_field %>% 
  rename(`attached` = `total_attached_field`, `motile` = `total_motile_field`) %>% 
  gather(`attached`, `motile`, key = "stage", value = "count") %>% 
  mutate(method = paste("field"))

lab_vs_field_tidy <- rbind(all_lab_tidy,all_field_tidy)

lab_vs_field_summary <- lab_vs_field_tidy %>% 
  group_by(method,stage) %>% 
  summarise(N = n(),
            mean_count = mean(count),
            sd = sd(count),
            se = sd/sqrt(N))

save(lab_vs_field_summary, file = "lab_vs_field_summary.RData")

plot_lab_vs_field <- ggplot(lab_vs_field_summary, aes(x=stage, y=mean_count, fill=method)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_count-se, ymax=mean_count+se),
                width=.2,
                position=position_dodge(0.9))
plot_lab_vs_field


# Stats
shapiro.test(lab_vs_field$t_attached_lab)
shapiro.test(lab_vs_field$t_attached_field)
shapiro.test(lab_vs_field$t_motile_lab)

wilcox.test(lab_vs_field$total_attached_lab, lab_vs_field$total_attached_field, paired = TRUE)
wilcoxsign_test(lab_vs_field$total_attached_lab ~ lab_vs_field$total_attached_field, distribution = "exact")
wilcox.test(lab_vs_field$total_motile_lab, lab_vs_field$total_motile_field, paired = TRUE)
wilcoxsign_test(lab_vs_field$total_motile_lab ~ lab_vs_field$total_motile_field, distribution="exact")


# Compare total counts by life stage ------------------------------------------
cope <- gs_read(sealice, ws = "cope")
chal <- gs_read(sealice, ws = "chal")
cal_mot <- gs_read(sealice, ws = "cal_mot")
lep_mot <- gs_read(sealice, ws = "lep_mot")

# Tidy the data frames
cal_cope <- cope %>%
  select(ufn, cal_cope_field, cal_cope_lab) %>%
  rename(`field` = `cal_cope_field`, `lab_fine` = `cal_cope_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("cal_cope"))
cal_cope <- cal_cope[c(1,4,2,3)]

lep_cope <- cope %>%
  select(ufn, lep_cope_field, lep_cope_lab) %>% 
  rename(`field` = `lep_cope_field`, `lab_fine` = `lep_cope_lab`) %>% 
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("lep_cope"))
lep_cope <- lep_cope[c(1,4,2,3)]

cope_tidy <- rbind(cal_cope, lep_cope)

# Tidy the chal data frame
chal_a <- chal %>%
  select(ufn, chal_a_field, chal_a_lab) %>%
  rename(`field` = `chal_a_field`, `lab_fine` = `chal_a_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("chal_a"))
chal_a <- chal_a[c(1,4,2,3)]

chal_b <- chal %>%
  select(ufn, chal_b_field, chal_b_lab) %>% 
  rename(`field` = `chal_b_field`, `lab_fine` = `chal_b_lab`) %>% 
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("chal_b"))
chal_b <- chal_b[c(1,4,2,3)]

unid_chal <- chal %>%
  select(ufn, unid_chal_field, unid_chal_lab) %>% 
  rename(`field` = `unid_chal_field`, `lab_fine` = `unid_chal_lab`) %>% 
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("unid_chal"))
unid_chal <- unid_chal[c(1,4,2,3)]

chal_tidy <- rbind(chal_a, chal_b, unid_chal)

# Tidy the cal_mot data frame
cmot <- cal_mot %>%
  select(ufn, cal_mot_field, cal_mot_lab) %>%
  rename(`field` = `cal_mot_field`, `lab_fine` = `cal_mot_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("cal_mot"))
cmot <- cmot[c(1,4,2,3)]

cgf <- cal_mot %>%
  select(ufn, cgf_field, cgf_lab) %>%
  rename(`field` = `cgf_field`, `lab_fine` = `cgf_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("cgf"))
cgf <- cgf[c(1,4,2,3)]

cal_mot_tidy <- rbind(cmot, cgf)

# Tidy the lep_mot data frame
lpam <- lep_mot %>%
  select(ufn, lpam_field, lpam_lab) %>%
  rename(`field` = `lpam_field`, `lab_fine` = `lpam_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("lpam"))
lpam <- lpam[c(1,4,2,3)]

lpaf <- lep_mot %>%
  select(ufn, lpaf_field, lpaf_lab) %>%
  rename(`field` = `lpaf_field`, `lab_fine` = `lpaf_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("lpaf"))
lpaf <- lpaf[c(1,4,2,3)]

lam <- lep_mot %>%
  select(ufn, lam_field, lam_lab) %>%
  rename(`field` = `lam_field`, `lab_fine` = `lam_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("lam"))
lam <- lam[c(1,4,2,3)]

laf <- lep_mot %>%
  select(ufn, laf_field, laf_lab) %>%
  rename(`field` = `laf_field`, `lab_fine` = `laf_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("laf"))
laf <- laf[c(1,4,2,3)]

lgf <- lep_mot %>%
  select(ufn, lgf_field, lgf_lab) %>%
  rename(`field` = `lgf_field`, `lab_fine` = `lgf_lab`) %>%
  gather(`field`, `lab_fine`, key = "method", value = "count") %>% 
  mutate(class = paste("lgf"))
lgf <- lgf[c(1,4,2,3)]

lep_mot_tidy <- rbind(lpam, lpaf, lam, laf, lgf)


# Combine all data frames and create summary statistics
sealice_tidy <- rbind(cope_tidy,chal_tidy,cal_mot_tidy,lep_mot_tidy)

sealice_summary <- sealice_tidy %>% 
  group_by(method,class) %>% 
  summarise(N = n(),
            mean_count = mean(count),
            sd = sd(count),
            se = sd/sqrt(N))

save(sealice_summary, file = "all_lice_summary.RData")


# Plot the data
plot_all_lice <- ggplot(sealice_summary, aes(x=class, y=mean_count, fill=method)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_count-se, ymax=mean_count+se),
                width=.2,
                position=position_dodge(0.9))
plot_all_lice


# Run the stats
cope_stats <- cope %>%
  mutate(tot_cope_lab = cal_cope_lab + lep_cope_lab,
         tot_cope_field = cal_cope_field + lep_cope_field,
         copepodid = tot_cope_lab - tot_cope_field) %>% 
  filter(!(tot_cope_lab == 0 & tot_cope_field == 0))
shapiro.test(cope_stats$cal_cope_lab)
shapiro.test(cope_stats$cal_cope_field)
wilcox.test(cope_stats$cal_cope_lab, cope_stats$cal_cope_field, paired = TRUE)

cmot_stats <- cal_mot %>%
  mutate(tot_cal_mot_lab = cal_mot_lab + cgf_lab,
         tot_cal_mot_field = cal_mot_field + cgf_field) %>%
  filter(!(tot_cal_mot_lab == 0 & tot_cal_mot_field == 0))
shapiro.test(cmot_stats$cal_mot_lab)
shapiro.test(cmot_stats$cal_mot_field)
wilcox.test(cmot_stats$cal_mot_lab, cmot_stats$cal_mot_field, paired = TRUE)

chal_stats <- chal %>%
  mutate(
    tot_chal_lab = chal_a_lab + chal_b_lab + unid_chal_lab,
    tot_chal_field = chal_a_field + chal_b_field + unid_chal_field
  ) %>%
  filter(!(tot_chal_lab == 0 & tot_chal_field == 0))
wilcox.test(chal_stats$chal_a_lab, chal_stats$chal_a_field, paired = TRUE)
wilcox.test(chal_stats$chal_b_lab, chal_stats$chal_b_field, paired = TRUE)


# Calculate mean differences --------------------------------------------------

cope_found <- cope %>%
  mutate(
    tot_cope_lab = cal_cope_lab + lep_cope_lab,
    tot_cope_field = cal_cope_field + lep_cope_field,
    copepodid = tot_cope_lab - tot_cope_field) %>% 
  filter(!(tot_cope_lab == 0 & tot_cope_field == 0)) %>%  #Filters out fish in which no lice were found
  mutate(copepodid = tot_cope_lab - tot_cope_field) %>% 
  select(ufn, copepodid) %>% 
  gather(`copepodid`, key = "class", value = "diff")

chal_found <- chal %>%
  mutate(
    tot_chal_lab = chal_a_lab + chal_b_lab + unid_chal_lab,
    tot_chal_field = chal_a_field + chal_b_field + unid_chal_field
  ) %>%
  filter(!(tot_chal_lab == 0 & tot_chal_field == 0)) %>% #Filters out fish in which no lice were found
  mutate(chalimus = tot_chal_lab - tot_chal_field) %>%  
  select(ufn, chalimus) %>%
  gather(`chalimus`, key = "class", value = "diff")

non_motiles <- rbind(cope_found, chal_found)

cal_mot_found <- cal_mot %>%
  mutate(tot_cal_mot_lab = cal_mot_lab + cgf_lab,
         tot_cal_mot_field = cal_mot_field + cgf_field) %>%
  filter(!(tot_cal_mot_lab == 0 & tot_cal_mot_field == 0)) %>%  #Filters out fish in which no lice were found
  mutate(cal_motiles = tot_cal_mot_lab - tot_cal_mot_field) %>% 
  select(ufn, cal_motiles) %>%
  gather(`cal_motiles`, key = "class", value = "diff")

lep_preadult_found <- lep_mot %>% 
  mutate(tot_lep_preadult_lab = lpam_lab + lpaf_lab,
         tot_lep_preadult_field = lpam_field + lpaf_field) %>% 
  filter(!(tot_lep_preadult_lab == 0 & tot_lep_preadult_field == 0)) %>% #Filters out fish in which no lice were found
  mutate(lep_preadult = tot_lep_preadult_lab - tot_lep_preadult_field) %>% 
  select(ufn, lep_preadult) %>% 
  gather(`lep_preadult`, key = "class", value = "diff")

lep_adult_found <- lep_mot %>% 
  mutate(tot_lep_adult_lab = laf_lab + lam_lab + lgf_lab,
         tot_lep_adult_field = laf_field + lam_field + lgf_field) %>% 
  filter(!(tot_lep_adult_lab == 0 & tot_lep_adult_field == 0)) %>% #Filters out fish in which no lice were found
  mutate(lep_adult = tot_lep_adult_lab - tot_lep_adult_field) %>% 
  select(ufn, lep_adult) %>% 
  gather(`lep_adult`, key = "class", value = "diff")

motiles <- rbind(cal_mot_found,lep_preadult_found,lep_adult_found)

##Without filtering out fish with zero counts for both methods
# motiles <- left_join(cal_mot, lep_mot) %>%  
#   mutate(
#     tot_cal_mot_lab = cal_mot_lab + cgf_lab,
#     tot_lep_preadult_lab = lpam_lab + lpaf_lab,
#     tot_lep_adult_lab = laf_lab + lam_lab + lgf_lab,
#     tot_cal_mot_field = cal_mot_field + cgf_field,
#     tot_lep_preadult_field = lpam_field + lpaf_field,
#     tot_lep_adult_field = laf_field + lam_field + lgf_field
#   ) %>%
#   mutate(
#     cal_motiles = tot_cal_mot_lab - tot_cal_mot_field,
#     lep_preadult = tot_lep_preadult_lab - tot_lep_preadult_field,
#     lep_adult = tot_lep_adult_lab - tot_lep_adult_field
#   ) %>%
# select(ufn, cal_motiles, lep_preadult, lep_adult) %>%
#   gather(`cal_motiles`,
#          `lep_preadult`,
#          `lep_adult`,
#          key = "class",
#          value = "diff")

count_differences <- rbind(non_motiles, motiles)

count_differences_summary <- count_differences %>% 
  group_by(class) %>% 
  summarise(N = n(),
            mean_diff = mean(diff),
            sd= sd(diff),
            se = sd/sqrt(N))

save(count_differences_summary, file = "count_differences_summary.RData")

plot_differences <- ggplot(count_differences_summary, aes(x=class, y=mean_diff)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean_diff-se, ymax=mean_diff+se),
                width=.2,
                position=position_dodge(0.9))
plot_differences
