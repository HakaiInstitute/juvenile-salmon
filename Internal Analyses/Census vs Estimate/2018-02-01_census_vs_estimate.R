library(tidyverse)
library(googlesheets4)
library(hakaiR)

census <- read_sheet('1PfbS_hhGlTeil2N_C9HUxDa3ZT_-CpcWrodQgUlHLQM', sheet = "Sheet1")


# plot linear regression of estimate vs census
linear_plot <- ggplot(data = census, mapping = aes(x = census_in_net, y = estimate_in_net))+
  #add point to plot
  geom_point() + 
  coord_cartesian(ylim = c(0,2000), xlim = c(0,2000)) +
  geom_smooth(method = "lm") +
  xlab("Census count") +
  ylab("Visual estimate") +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000))

linear_plot

ggsave('linear_regression.png')



lm <- lm(estimate_in_net~census_in_net, data=census)
summary(lm)
plot(lm)

census_diff <- census %>% 
  mutate(diff = abs(census_in_net - estimate_in_net))

# plot the linear regression between the difference in census and the estimate vs the actual census
diff_linear_plot <- ggplot(data = census_diff, mapping = aes(x = census_in_net, y = diff)) +
  geom_point() +
  geom_smooth(method = "lm")
diff_linear_plot

diff_lm = lm(diff~census_in_net, data=census)
summary(diff_lm)
plot(diff_lm)

ggsave('diff_linear_plot.png')

# Definitely there's a greater error at larger values of census counts
# Therefore I will try a segmented regression

plot(lm)

library(segmented)
lm_piecewise <- lm(estimate_in_net ~ census_in_net, data=census)
segmented_model <- segmented(lm_piecewise, seg.Z = ~census_in_net)
summary(segmented_model)
plot(segmented_model)
lines(segmented_model)
plot(census$estimate_in_net, residuals(segmented_model))
