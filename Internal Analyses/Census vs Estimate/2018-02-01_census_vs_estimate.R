library(tidyverse)
library(googlesheets)

census_workbook <- gs_key('1PfbS_hhGlTeil2N_C9HUxDa3ZT_-CpcWrodQgUlHLQM', visibility="private", lookup = FALSE)
census <- gs_read(census_workbook, ws = 'Sheet1')

# plot linear regression of estimate vs census
linear_plot <- ggplot(data = census, mapping = aes(x = estimate_in_net, y = census_in_net))+
  #add point to plot
  geom_point() + 
  geom_smooth(method = lm)
ggsave('linear_regression.png')

linear_plot

lm <- lm(estimate_in_net~census_in_net, data=census)
summary(lm)
plot(lm)

census <- census %>% 
  mutate(diff = census_in_net - estimate_in_net)

# plot the linear regression between the difference in census and the estimate vs the actual census
diff_linear_plot <- ggplot(data = census, mapping = aes(x = census_in_net, y = diff)) +
  geom_point() +
  geom_smooth(method = "auto")
diff_linear_plot

diff_lm = lm(diff~census_in_net, data=census)
summary(diff_lm)
plot(diff_lm)

ggsave('diff_linear_plot.png')
