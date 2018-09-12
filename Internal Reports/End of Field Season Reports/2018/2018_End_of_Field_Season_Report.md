2018 End of Season Report
================
Julian Gan, Brett Johnson, and Sara Tremblay-Boyer
11 September 2018

``` r
db <- gs_key("1hTC60Nc60k23rMMzcV9clPo4mcR1iqQYgKR2zD6nOfQ", 
                                   lookup = F, visibility = "private")
```

    ## Worksheets feed constructed with private visibility

``` r
surveys <- gs_read(db, ws = "survey_data")
```

    ## Accessing worksheet titled 'survey_data'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   cloud_cover = col_integer(),
    ##   sea_state = col_integer(),
    ##   survey_time_start = col_time(format = ""),
    ##   survey_time_end = col_time(format = ""),
    ##   secchi = col_double(),
    ##   net_sets = col_integer()
    ## )

    ## See spec(...) for full column specifications.

``` r
seines <- gs_read(db, ws = "seine_data")
```

    ## Accessing worksheet titled 'seine_data'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   seine_id = col_character(),
    ##   survey_id = col_character(),
    ##   set_type = col_character(),
    ##   set_time = col_time(format = ""),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   fish_retained = col_character(),
    ##   collection_protocol = col_character(),
    ##   seine_comments = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
survey_seines_2018 <- full_join(surveys, seines, by = "survey_id") %>% 
  filter(survey_date >= as.Date("2018-05-01"))
write_csv(survey_seines_2018, here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "survey_seines_2018.csv"))
survey_seines_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "survey_seines_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   cloud_cover = col_integer(),
    ##   sea_state = col_integer(),
    ##   survey_time_start = col_time(format = ""),
    ##   survey_time_end = col_time(format = ""),
    ##   net_sets = col_integer(),
    ##   set_number = col_integer(),
    ##   set_time = col_time(format = ""),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   set_sliders = col_integer(),
    ##   set_poppers = col_integer(),
    ##   set_dimpling = col_integer(),
    ##   so_taken = col_integer(),
    ##   so_total = col_integer(),
    ##   pi_taken = col_integer(),
    ##   pi_total = col_integer(),
    ##   cu_taken = col_integer(),
    ##   cu_total = col_integer(),
    ##   co_taken = col_integer()
    ##   # ... with 5 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
packages <- gs_read(db, ws = "package_data")
```

    ## Accessing worksheet titled 'package_data'.

    ## Parsed with column specification:
    ## cols(
    ##   package_id = col_character(),
    ##   seine_id = col_character(),
    ##   pkg_species = col_character(),
    ##   number_of_fish = col_character(),
    ##   field_liced_status = col_character(),
    ##   pkg_time_out = col_time(format = ""),
    ##   pkg_time_dewar = col_time(format = ""),
    ##   pkg_preservation_status = col_character(),
    ##   pkg_location = col_character(),
    ##   freezer_type = col_integer(),
    ##   freezer_number = col_character(),
    ##   freezer_shelf = col_character(),
    ##   freezer_section = col_character(),
    ##   pkg_comments = col_character()
    ## )

``` r
packages_2018 <- left_join(packages, survey_seines_2018, by = "seine_id") %>% 
  filter(survey_date >= as.Date("2018-05-01")) 
packages_2018 <- packages_2018[c(1:14)]
write_csv(packages_2018, here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "packages.csv"))
packages_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "packages.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   package_id = col_character(),
    ##   seine_id = col_character(),
    ##   pkg_species = col_character(),
    ##   number_of_fish = col_integer(),
    ##   field_liced_status = col_character(),
    ##   pkg_time_out = col_time(format = ""),
    ##   pkg_time_dewar = col_time(format = ""),
    ##   pkg_preservation_status = col_integer(),
    ##   pkg_location = col_character(),
    ##   freezer_type = col_integer(),
    ##   freezer_number = col_integer(),
    ##   freezer_shelf = col_integer(),
    ##   freezer_section = col_character(),
    ##   pkg_comments = col_character()
    ## )

``` r
fish <- gs_read(db, ws = "fish_field_data")
```

    ## Accessing worksheet titled 'fish_field_data'.

    ## Parsed with column specification:
    ## cols(
    ##   semsp_id = col_character(),
    ##   ufn = col_character(),
    ##   seine_id = col_character(),
    ##   species = col_character(),
    ##   package_id = col_character(),
    ##   fish_time_out = col_character(),
    ##   fish_time_dewar = col_character(),
    ##   fork_length_field = col_character(),
    ##   height_field = col_character(),
    ##   weight_field = col_character(),
    ##   analysis_planned = col_character(),
    ##   dissection_status = col_character(),
    ##   lice_qc_flag = col_character(),
    ##   qc_comments = col_character()
    ## )

``` r
fish_2018 <- left_join(fish,survey_seines_2018) %>% 
  filter(survey_date >= as.Date("2018-05-01"))
```

    ## Joining, by = "seine_id"

``` r
fish_2018 <- fish_2018[c(2:14,16:18)]
write_csv(fish_2018, here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "fish_2018.csv"))
fish_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "fish_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   ufn = col_character(),
    ##   seine_id = col_character(),
    ##   species = col_character(),
    ##   package_id = col_character(),
    ##   fish_time_out = col_time(format = ""),
    ##   fish_time_dewar = col_time(format = ""),
    ##   fork_length_field = col_integer(),
    ##   height_field = col_integer(),
    ##   weight_field = col_double(),
    ##   analysis_planned = col_character(),
    ##   dissection_status = col_character(),
    ##   lice_qc_flag = col_character(),
    ##   qc_comments = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   site_id = col_character(),
    ##   region = col_character()
    ## )

``` r
sealice_field <- gs_read(db, ws = "sealice_field")
```

    ## Accessing worksheet titled 'sealice_field'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ufn = col_character(),
    ##   comments_fish_health_field = col_character(),
    ##   licing_protocol_field = col_character(),
    ##   qc_flag = col_character(),
    ##   flag_reason = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
sealice_field_2018 <- left_join(fish_2018,sealice_field, by = "ufn")
write_csv(sealice_field_2018, here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "sealice_field_2018.csv"))
sealice_field_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "2018", "Raw Data", "sealice_field_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ufn = col_character(),
    ##   seine_id = col_character(),
    ##   species = col_character(),
    ##   package_id = col_character(),
    ##   fish_time_out = col_time(format = ""),
    ##   fish_time_dewar = col_time(format = ""),
    ##   weight_field = col_double(),
    ##   analysis_planned = col_character(),
    ##   dissection_status = col_character(),
    ##   lice_qc_flag = col_character(),
    ##   qc_comments = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   site_id = col_character(),
    ##   region = col_character(),
    ##   comments_fish_health_field = col_character(),
    ##   licing_protocol_field = col_character(),
    ##   qc_flag = col_character(),
    ##   flag_reason = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
sreg <- survey_seines_2018 %>% 
  dplyr::select(set_sliders, so_total, cu_total, pi_total)

sreg$set_sliders <- as.factor(sreg$set_sliders)
slider <- sreg$set_sliders
sototal <- sreg$so_total

### Perforoming Ordered Logisitic Regression on slider activity and total sockeye catch.
slider_so_logit <- polr(slider ~ sototal, data = sreg, Hess = T)
summary (slider_so_logit)
```

    ## Call:
    ## polr(formula = slider ~ sototal, data = sreg, Hess = T)
    ## 
    ## Coefficients:
    ##            Value Std. Error t value
    ## sototal 0.001617   0.001533   1.055
    ## 
    ## Intercepts:
    ##      Value  Std. Error t value
    ## 0|1  0.5724 0.2381     2.4042 
    ## 1|10 2.1490 0.3637     5.9096 
    ## 
    ## Residual Deviance: 141.0541 
    ## AIC: 147.0541 
    ## (22 observations deleted due to missingness)

``` r
oddsratio <- exp(coef(slider_so_logit)) #coeffecients are log odds so I exponentitate them (with base e) to get odds ratio 
oddsratio
```

    ##  sototal 
    ## 1.001618

``` r
##storing table
(slider_so_table <- coef(summary(slider_so_logit)))
```

    ##               Value  Std. Error  t value
    ## sototal 0.001616953 0.001533142 1.054666
    ## 0|1     0.572445208 0.238102211 2.404199
    ## 1|10    2.149039543 0.363655338 5.909550

``` r
slider_so_p <- pnorm(abs(slider_so_table[,"t value"]), lower.tail = FALSE) * 2
(slider_so_table <- cbind(slider_so_table, "p value" = slider_so_p))
```

    ##               Value  Std. Error  t value      p value
    ## sototal 0.001616953 0.001533142 1.054666 2.915782e-01
    ## 0|1     0.572445208 0.238102211 2.404199 1.620793e-02
    ## 1|10    2.149039543 0.363655338 5.909550 3.430427e-09

``` r
(slider_so_ci <- confint(slider_so_logit))
```

    ## Waiting for profiling to be done...

    ##         2.5 %        97.5 % 
    ## -0.0009570266  0.0056524157

``` r
exp(cbind(OR = coef(slider_so_logit), slider_so_ci))
```

    ##              OR slider_so_ci
    ## 2.5 %  1.001618    0.9990434
    ## 97.5 % 1.001618    1.0056684

``` r
detach("package:MASS", unload = TRUE)
```

``` r
sea_lice_duration_out <- sealice_field_2018 %>% 
  filter(licing_protocol_field == "salmoncoast_allstages") %>% 
  select(ufn, region, fish_time_out, fish_time_dewar) %>% 
  mutate(duration_out = (fish_time_dewar - fish_time_out)/60)

(mean(sea_lice_duration_out$duration_out))
```

    ## Time difference of 14.52841 secs

``` r
ggplot(sea_lice_duration_out, aes(x = duration_out)) +
  geom_histogram(binwidth = 2)+
  xlim(2,25) +
  xlab("Duration Out (minutes)")+
  ylab("Count")
```

    ## Warning: Removed 11 rows containing non-finite values (stat_bin).

![](2018_End_of_Field_Season_Report_files/figure-markdown_github/time%20each%20of%20the%20first%20ten%20sockeye%20are%20out%20of%20the%20water-1.png)

``` r
ggsave(here("Internal Reports", "End of Field Season Reports", "2018", "Figures", "sealice_field_2018.png"), width = 5, height = 3)
```

    ## Warning: Removed 11 rows containing non-finite values (stat_bin).

``` r
library(ggplot2)
library(wesanderson)

survey_seines <- full_join(surveys,seines, by = "survey_id")
fish_survey_seines <- left_join(fish,survey_seines)
```

    ## Joining, by = "seine_id"

``` r
all_fish_lice <- full_join(fish_survey_seines, sealice_field, by = "ufn") %>% 
  filter(licing_protocol_field != "NA") %>% 
  filter(species == "SO" | species == "PI" | species=="CU") %>% 
  mutate(survey_year = as.character(year(survey_date))) %>% 
  select(ufn,species,region,survey_year) %>% 
  group_by(species,survey_year)

summarise(all_fish_lice, count=n())
```

    ## # A tibble: 6 x 3
    ## # Groups:   species [?]
    ##   species survey_year count
    ##   <chr>   <chr>       <int>
    ## 1 CU      2017          156
    ## 2 CU      2018          347
    ## 3 PI      2017           28
    ## 4 PI      2018          379
    ## 5 SO      2017          284
    ## 6 SO      2018          176

``` r
ggplot(data = all_fish_lice) + 
  geom_bar(mapping = aes(x=species, fill=survey_year), position=position_dodge()) + 
  scale_fill_manual(name="Survey Year", values = wes_palette("Zissou1")) +
  xlab("Species") +
  ylab("Count")
```

![](2018_End_of_Field_Season_Report_files/figure-markdown_github/fish%20loused%20in%202018%20vs.%202017-1.png)

``` r
ggsave(here("Internal Reports", "End of Field Season Reports", "2018", "Figures", "comparison_fish_loused_2017-18.png"), width = 5, height = 3)
```

``` r
# Summary table of number of each species retained
field_2018_sites_spp_summary <- survey_seines_2018 %>%
  select(survey_date, site_id, so_taken, pi_taken, cu_taken, co_taken, he_taken, ck_taken, region, zone) %>%
   rename(chinook = ck_taken,
         coho = co_taken,
         chum = cu_taken,
         herring = he_taken,
         pink = pi_taken,
         sockeye = so_taken) %>% 
  gather(sockeye, pink, chum, coho, chinook, herring, key = "species", value = "n") %>% 
  replace_na(list( n = 0)) %>% na.omit()

species_summary <- field_2018_sites_spp_summary %>%
  group_by(species) %>% 
  summarize(n = sum(n)) %>% 
  rename(total = n) %>% 
  arrange(desc(total))

knitr::kable(species_summary)
```

| species |  total|
|:--------|------:|
| pink    |    379|
| sockeye |    379|
| chum    |    347|
| herring |     52|
| coho    |     39|
| chinook |      9|

``` r
# Properly ordering the facets
field_2018_sites_spp_summary$species <-
  factor(field_2018_sites_spp_summary$species, levels = c("sockeye", "chum", "pink", "coho", "chinook", "herring"))

#making the graph of nbr of fish caught for each species in each zone
#TO IMPROVE: getting rid of the double seines points

d <- ggplot(field_2018_sites_spp_summary, aes(x = survey_date, y = zone), group = species) +
  geom_count(aes( size = n, alpha = n)) +
  facet_wrap(~species) +
  scale_y_discrete(limits= c("N", "S", "W","C", "E"),
    breaks= c("W", "S", "N", "E", "C"), 
    labels= c("West", "South", "North", "East", "Central")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(color = "gray56"), axis.ticks.x = element_line(color = "gray56")) + scale_size_continuous(range = c(0.5,3.5))
d
```

![](2018_End_of_Field_Season_Report_files/figure-markdown_github/Catch%20Statistics-1.png)

``` r
ggsave("species_distribution.png", d)  
```

    ## Saving 7 x 5 in image

``` r
# looking at the distribution of sockeye retained by each migration zone. Discovery Island versus. Johnstone Strait

sockeye_distribution <- survey_seines_2018 %>%
   select(survey_date, region, zone, so_taken) %>%
  arrange(region, zone) %>%
  rename(sockeye = so_taken) %>% 
  na.omit()

# TO IMPROVE: add May ?

s_colors <- brewer.pal(n = 5, name = "Dark2")
s <- ggplot(sockeye_distribution, aes(x = survey_date, y = zone, color = region)) + geom_count(aes( size = sockeye)) +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values = s_colors,
                     name = "",
                     breaks= c("DI", "JS"),
labels=c("Discovery Islands", "Jonhstone Strait")) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(color = "gray56"), axis.ticks.x = element_line(color = "gray56")) +
  scale_y_discrete(limits= c("N", "S", "W","C", "E"),
    breaks= c("W", "S", "N", "E", "C"),
                   labels= c("West", "South", "North", "East", "Central"))
s
```

![](2018_End_of_Field_Season_Report_files/figure-markdown_github/Catch%20Statistics-2.png)

``` r
ggsave("sockeye_distribution.png", s)
```

    ## Saving 7 x 5 in image

``` r
#FOR FUN... maybe by site_id instead of region 

for_fun <- survey_seines_2018 %>%
select(survey_date, region, zone, site_id, so_taken) %>%
arrange(region, zone, site_id) %>%
rename(sockeye = so_taken) %>% 
  na.omit()

#changing facet labels and order of sites
region_labels <- c(DI = "Discovery Islands", JS = "Johnstone Strait")

for_fun$site_id <- factor(for_fun$site_id, levels = c("J11", "J09", "J03","J02","D10","D30", "D09","D07", "D27", "D22", "D08"))

#The graph 
f <- ggplot(for_fun, aes(x = survey_date, y = site_id, color = zone)) + geom_count(aes( size = sockeye), alpha = 1/ 2) +
labs(x = NULL, y = NULL)+
  scale_color_manual(values = s_colors,
                     name = "Zone",
                     breaks= c("W","C","E","N", "S"),
labels=c("West", "Central", "East", "North", "South")) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(color = "gray56"), axis.ticks.x = element_line(color = "gray56")) +
   facet_wrap(~region, nrow = 2, scales = "free_y", labeller = labeller( region = region_labels))+
  scale_size_continuous(range = c(1,5))
f
```

![](2018_End_of_Field_Season_Report_files/figure-markdown_github/Catch%20Statistics-3.png)

``` r
ggsave("site_id_sockeye.png",f)
```

    ## Saving 7 x 5 in image
