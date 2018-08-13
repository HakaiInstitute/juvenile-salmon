2018 End of Season Report
================
Sara Tremblay-Boyer
9 août 2018

Loading data:
=============

``` r
field_data_workbook_2018 <- gs_key("1iRQCKMTznHbMeI9GunbsgPyb-rtWgwj-0IfxLw7NBJE", 
                                   lookup = F, visibility = "private")
```

    ## Worksheets feed constructed with private visibility

    ## Auto-refreshing stale OAuth token.

``` r
surveys <- gs_read(field_data_workbook_2018, ws = "survey_data")
```

    ## Accessing worksheet titled 'survey_data'.

    ## Parsed with column specification:
    ## cols(
    ##   survey_id = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   site_id = col_character(),
    ##   region = col_character(),
    ##   zone = col_character(),
    ##   site_name = col_character(),
    ##   crew = col_character(),
    ##   precip = col_character(),
    ##   cloud_cover = col_integer(),
    ##   sea_state = col_integer(),
    ##   wind_speed = col_character(),
    ##   wind_direction = col_character(),
    ##   tide_state = col_character(),
    ##   survey_time_start = col_time(format = ""),
    ##   survey_time_end = col_time(format = ""),
    ##   survey_type = col_character(),
    ##   net_sets = col_integer(),
    ##   survey_comments = col_character(),
    ##   lab_qc = col_character()
    ## )

``` r
seines <- gs_read(field_data_workbook_2018, ws = "seine_data")
```

    ## Accessing worksheet titled 'seine_data'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   survey_id = col_character(),
    ##   seine_id = col_character(),
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
survey_seines_2018 <- full_join(surveys, seines, by = "survey_id")
write_csv(survey_seines_2018, here("Internal Reports", "End of Field Season Reports", "Raw Data", "survey_seines_2018.csv"))
survey_seines_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "Raw Data", "survey_seines_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   survey_id = col_character(),
    ##   survey_date = col_date(format = ""),
    ##   site_id = col_character(),
    ##   region = col_character(),
    ##   zone = col_character(),
    ##   site_name = col_character(),
    ##   crew = col_character(),
    ##   precip = col_character(),
    ##   wind_speed = col_character(),
    ##   wind_direction = col_character(),
    ##   tide_state = col_character(),
    ##   survey_time_start = col_time(format = ""),
    ##   survey_time_end = col_time(format = ""),
    ##   survey_type = col_character(),
    ##   survey_comments = col_character(),
    ##   lab_qc = col_character(),
    ##   seine_id = col_character(),
    ##   set_type = col_character(),
    ##   set_time = col_time(format = ""),
    ##   lat = col_double()
    ##   # ... with 4 more columns
    ## )
    ## See spec(...) for full column specifications.

``` r
packages_2018 <- gs_read(field_data_workbook_2018, ws = "package_data")
```

    ## Accessing worksheet titled 'package_data'.

    ## Parsed with column specification:
    ## cols(
    ##   package_id = col_character(),
    ##   seine_id = col_character(),
    ##   species = col_character(),
    ##   number_of_fish = col_integer(),
    ##   whatman_sheet = col_character(),
    ##   whatman_cell_start = col_integer(),
    ##   field_liced_status = col_character(),
    ##   time_out = col_time(format = ""),
    ##   time_dewar = col_time(format = ""),
    ##   field_dewar_number = col_character(),
    ##   transport_dewar_number = col_character(),
    ##   pkg_preservation_status = col_integer(),
    ##   freezer_type = col_integer(),
    ##   freezer_number = col_integer(),
    ##   freezer_shelf = col_integer(),
    ##   package_comments = col_character(),
    ##   duplicate_Id_flag = col_character(),
    ##   lab_qc = col_character()
    ## )

``` r
write_csv(packages_2018, here("Internal Reports", "End of Field Season Reports", "Raw Data", "packages.csv"))
packages_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "Raw Data", "packages.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   package_id = col_character(),
    ##   seine_id = col_character(),
    ##   species = col_character(),
    ##   number_of_fish = col_integer(),
    ##   whatman_sheet = col_character(),
    ##   whatman_cell_start = col_integer(),
    ##   field_liced_status = col_character(),
    ##   time_out = col_time(format = ""),
    ##   time_dewar = col_time(format = ""),
    ##   field_dewar_number = col_character(),
    ##   transport_dewar_number = col_character(),
    ##   pkg_preservation_status = col_integer(),
    ##   freezer_type = col_integer(),
    ##   freezer_number = col_integer(),
    ##   freezer_shelf = col_integer(),
    ##   package_comments = col_character(),
    ##   duplicate_Id_flag = col_character(),
    ##   lab_qc = col_character()
    ## )

``` r
fish_2018 <- gs_read(field_data_workbook_2018, ws = "fish_and_sealice_field_data")
```

    ## Accessing worksheet titled 'fish_and_sealice_field_data'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ufn = col_character(),
    ##   species = col_character(),
    ##   seine_id = col_character(),
    ##   package_id = col_character(),
    ##   time_out = col_time(format = ""),
    ##   time_dewar = col_time(format = ""),
    ##   weight = col_double(),
    ##   field_liced = col_character(),
    ##   comments = col_character(),
    ##   duplicate_id_flag = col_character(),
    ##   lab_qc = col_character(),
    ##   analysis_planned = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
fish_2018 <- fish_2018[c(1:9,37)]
write_csv(fish_2018, here("Internal Reports", "End of Field Season Reports", "Raw Data", "fish_2018.csv"))
fish_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "Raw Data", "fish_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   ufn = col_character(),
    ##   species = col_character(),
    ##   seine_id = col_character(),
    ##   package_id = col_character(),
    ##   time_out = col_time(format = ""),
    ##   time_dewar = col_time(format = ""),
    ##   fork_length = col_integer(),
    ##   height = col_integer(),
    ##   weight = col_double(),
    ##   analysis_planned = col_character()
    ## )

``` r
sealice_field_2018 <- gs_read(field_data_workbook_2018, ws = "fish_and_sealice_field_data")
```

    ## Accessing worksheet titled 'fish_and_sealice_field_data'.

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ufn = col_character(),
    ##   species = col_character(),
    ##   seine_id = col_character(),
    ##   package_id = col_character(),
    ##   time_out = col_time(format = ""),
    ##   time_dewar = col_time(format = ""),
    ##   weight = col_double(),
    ##   field_liced = col_character(),
    ##   comments = col_character(),
    ##   duplicate_id_flag = col_character(),
    ##   lab_qc = col_character(),
    ##   analysis_planned = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
sealice_field_2018 <- sealice_field_2018[c(1:3,10:34,37)]
write_csv(sealice_field_2018, here("Internal Reports", "End of Field Season Reports", "Raw Data", "sealice_field_2018.csv"))
sealice_field_2018 <- read_csv(here("Internal Reports", "End of Field Season Reports", "Raw Data", "sealice_field_2018.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   ufn = col_character(),
    ##   species = col_character(),
    ##   seine_id = col_character(),
    ##   field_liced = col_character(),
    ##   comments = col_character(),
    ##   analysis_planned = col_character()
    ## )
    ## See spec(...) for full column specifications.

Catch Statistics:
=================

``` r
field_2018_sites_spp_summary <- survey_seines_2018 %>%
  select(survey_date, site_id, so_taken, pi_taken, cu_taken, co_taken, he_taken, ck_taken, region, zone) %>%
   rename(chinook = ck_taken,
         coho = co_taken,
         chum = cu_taken,
         herring = he_taken,
         pink = pi_taken,
         sockeye = so_taken) %>% 
  gather(sockeye, pink, chum, coho, chinook, herring, key = "species", value = "n") %>% 
  replace_na(list( n = 0)) 

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
