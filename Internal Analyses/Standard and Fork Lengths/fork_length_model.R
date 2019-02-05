library(tidyverse)
library(readxl)
library(ggplot2)

fish <- read_excel("Standard and Fork Lengths/2019-02-01_144827_HakaiData_jsp_fish.xlsx", sheet = "Hakai Data") %>% 
  mutate(fork_length = as.numeric(fork_length))

# modlen <- lm(fork_length ~ standard_length, data = fish)
# summary(modlen)
# coefficients(modlen)
# 
# ggplot(fish, aes(x=standard_length, y = fork_length)) +
#   geom_point(shape=1)+
#   geom_smooth(method=lm)

so_len <- fish %>% 
  filter(species == "SO") 

pi_len <- fish %>% 
  filter(species == "PI")

cu_len <- fish %>% 
  filter(species == "CU")

co_len <- fish %>% 
  filter(species == "CO")

ck_len <- fish %>% 
  filter(species == "CK")

he_len <- fish %>% 
  filter(species == "HE")

modlen_so <- lm(fork_length ~ standard_length, data = so_len)

modlen_pi <- lm(fork_length ~ standard_length, data = pi_len)

modlen_cu <- lm(fork_length ~ standard_length, data = cu_len)

modlen_co <- lm(fork_length ~ standard_length, data = co_len)

modlen_he <- lm(fork_length ~ standard_length, data = he_len)

modlen_ck <- lm(fork_length ~ standard_length, data = ck_len)

so_len_mod <- so_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_so$coef[2])*standard_length + as.numeric(modlen_so$coef[1])))

pi_len_mod <- pi_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_pi$coef[2])*standard_length + as.numeric(modlen_pi$coef[1])))

cu_len_mod <- cu_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_cu$coef[2])*standard_length + as.numeric(modlen_cu$coef[1])))

co_len_mod <- co_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_co$coef[2])*standard_length + as.numeric(modlen_co$coef[1])))

he_len_mod <- he_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_he$coef[2])*standard_length + as.numeric(modlen_he$coef[1])))

ck_len_mod <- ck_len %>%
  mutate(fork_length_modeled = round(as.numeric(modlen_ck$coef[2])*standard_length + as.numeric(modlen_ck$coef[1])))

fork_length_mod <- rbind(so_len_mod, pi_len_mod, cu_len_mod, co_len_mod, he_len_mod, ck_len_mod) %>% 
  filter(is.na(fork_length)) %>% 
  mutate(fork_length = coalesce(fork_length, fork_length_modeled)) %>% 
  select(hakai_id, species, standard_length, fork_length, comments) %>% 
  mutate(comments = "Fork length generated with species-specific linear regression model (2019-02-01 JG)")

write.csv(fork_length_mod, "Standard and Fork Lengths/2019-02-01_modeled_fork_lengths.csv")
