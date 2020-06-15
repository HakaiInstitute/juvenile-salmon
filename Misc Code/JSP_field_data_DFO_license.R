library(tidyverse)
library(googlesheets4)
library(lubridate)
library(sp)
library(here)

# Read in 2020 seine & survey data from Google sheets
surveys <- read_sheet("https://docs.google.com/spreadsheets/d/1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk/edit#gid=0", sheet = "survey_data")
seines <- read_sheet("https://docs.google.com/spreadsheets/d/1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk/edit#gid=0", sheet = "seine_data")
sites <- read_csv("https://raw.githubusercontent.com/HakaiInstitute/jsp-data/master/data/sites.csv")


# Source of code to transform decimal-degree lat/long coordinates to UTM: 
# https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
coordinates <- subset(seines, select = c(seine_id, lon, lat)) %>%  
  drop_na(seine_id) %>% 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat)) %>% 
  drop_na(lon, lat)

LongLatToUTM <- function(x, y, ID, zone){
  xy <- data.frame(ID = ID, X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone, " ellps=WGS84", sep='')))
  return(as.data.frame(res))
}

# Discovery Islands are in UTM zone 10; Johnstone Strait is zone 9
# If the data contains coordinates from both regions, split the data frame into DI & JS, 
# then apply the function to each frame independently.
convert <-  LongLatToUTM(coordinates$lon, coordinates$lat, ID = coordinates$seine_id, 10) %>% 
  rename(seine_id = ID,
         UTM_E = X,
         UTM_N = Y) %>% 
  mutate(zone = "10U",
         seine_id = as.character(seine_id))

# Join seine & survey tables
ss <- seines %>% 
  drop_na(seine_id) %>% 
  left_join(surveys, by = "survey_id") %>% 
  left_join(select(sites,
                   site_id,
                   pfma),
            by = "site_id")

# Create the "General Collection Data" sheet. Copy and paste the results into the Excel file.
general_collection_data <- ss %>% 
  pivot_longer(cols = so_taken:ck_total, names_to = "species", values_to = "count") %>% 
  separate(species, into = c("species", "taken_total"), sep = "_") %>% 
  pivot_wider(names_from = "taken_total", values_from = "count") %>% 
  filter(total > 0) %>% 
  left_join(convert, by = "seine_id") %>% 
  mutate(name = "Hakai Institute", # Make sure this is updated every year
         license_number = "XR 96 2020", # Make sure this is updated every year
         site = paste(site_id, site_name, sep = " - "),
         marine_freshwater = "Marine",
         watershed_code = "NA",
         sampling_date = format(as.Date(survey_date), format = "%d-%b-%Y"),
         sampling_time = format(as.POSIXct(strptime(set_time, 
                                                    format = "%Y-%m-%d %H:%M", 
                                                    tz = "America/Vancouver")
                                           ), 
                                format = "%H:%M"),
         collection_method = "purse seine",
         species = recode(species,
                          "so" = "sockeye",
                          "pi" = "pink",
                          "cu" = "chum",
                          "co" = "coho",
                          "ck" = "chinook",
                          "he" = "herring"
                          ),
         life_stage = "Juvenile",
         bio_sample = recode(fish_retained,
                             "yes" = "Yes",
                             "no" = "No"),
         comments = ""
         ) %>% 
  select(name,
         license_number,
         site,
         marine_freshwater,
         watershed_code,
         pfma,
         zone,
         Easting = UTM_E,
         Northing = UTM_N,
         sampling_date,
         sampling_time,
         collection_method,
         species,
         life_stage,
         no_retained = taken,
         no_released = total,
         bio_sample,
         comments = seine_comments
  )

write_csv(general_collection_data, here("data", "DFO_general_collection_data_2020.csv"))
# If there are notes in the data about fish being released in poor condition and/or dead, the numbers need to be manually adjusted.
# Make sure to add any bycatch/mortality data to the final spreadsheet.


# Read in 2020 fish & fin clip data from Google Sheets
fish <- read_sheet("https://docs.google.com/spreadsheets/d/1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk/edit#gid=1147885945", sheet = "fish_field_data")
finclips <- read_sheet("https://docs.google.com/spreadsheets/d/1F04L3heVXck9Co6THrE8vMzuu3O4zq4NwxC8FJdL5Uk/edit#gid=1147885945", sheet = "fin_clips")

# Create the "Biological Data" sheet.
biological_data <- left_join(fish, ss, by = "seine_id") %>% 
  drop_na(ufn) %>% 
  mutate(license_number = "XR 96 2020",
         sampling_date = format(as.Date(survey_date), format = "%d-%b-%Y"),
         sampling_time = format(as.POSIXct(strptime(set_time, 
                                                    format = "%Y-%m-%d %H:%M", 
                                                    tz = "America/Vancouver")
                                           ), 
         format = "%H:%M"),
         species = recode(species,
                          "SO" = "sockeye",
                          "PI" = "pink",
                          "CU" = "chum",
                          "CO" = "coho",
                          "CK" = "chinook",
                          "HE" = "herring"
                          ),
         retained_or_released = "Retained",
         fork_length_units = "millimetres (mm)",
         weight_units = "grams (g)",
         adipose_clipped = "no",
         fish_health = "good",
         comments = case_when(ufn %in% finclips$ufn ~ "Caudal fin clipped for genetic stock analysis")
  ) %>% 
  select(license_number,
         sampling_date,
         sampling_time,
         species,
         retained_or_released,
         fork_length_field,
         fork_length_units,
         weight_field,
         weight_units,
         adipose_clipped,
         fish_health, # Edit as necessary if the fish was caught in fair or poor condition
         comments
         )


write_csv(biological_data, here("data", "DFO_biological_data_2020.csv"))