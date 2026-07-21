# Load tidyverse library
library(tidyverse)

# Read in metro trains data
metro_train_stop_locations <- read.delim("data/gtfs/2/google_transit/stops.txt", header = TRUE, sep = ",")
metro_train_stop_times <- read.delim("data/gtfs/2/google_transit/stop_times.txt", header = TRUE, sep = ",")
metro_train_calendar_dates <- read.delim("data/gtfs/2/google_transit/calendar_dates.txt", header = TRUE, sep = ",")
metro_train_calendar <- read.delim("data/gtfs/2/google_transit/calendar.txt", header = TRUE, sep = ",")
metro_train_trips <- read.delim("data/gtfs/2/google_transit/trips.txt", header = TRUE, sep = ",")

train_weekday_services <- metro_train_calendar %>%
  filter(monday == 1 | tuesday == 1 | wednesday == 1 | thursday == 1 | friday == 1) %>%
  select(service_id)

train_services <- semi_join(metro_train_trips, train_weekday_services, by = "service_id")

train_stop_locations <- metro_train_stop_locations %>%
  # Keep rows where location type is not a station
  filter(location_type != 1 | is.na(location_type)) %>%
  # Keep only relevant rows
  select(stop_id, stop_name, parent_station)

# Create new parent stations df
train_parent_stations <- metro_train_stop_locations %>%
  # Keep only parent station data
  filter(location_type == 1) %>%
  # Keep relevant columns only
  select(stop_id, stop_name, stop_lat, stop_lon)
  
train_stop_frequencies <- semi_join(metro_train_stop_times, train_services, by = "trip_id")

trip_ids <- train_stop_frequencies %>%
  group_by(stop_id) %>%
  summarise(trip_id = unique(trip_id))

train_stop_frequencies <- train_stop_frequencies %>%
  # Group rows by stop_id
  group_by(stop_id) %>%
  # Count number of rows for each stop_id (number of services per stop)
  summarise(service_frequency = n_distinct(trip_id))

train_stop_frequencies$stop_id <- as.character(train_stop_frequencies$stop_id)

# Join stop frequency to stop location data
metro_train_stops <- left_join(train_stop_locations, train_stop_frequencies, by = "stop_id") %>%
  # Change NA service frequencies to 0
  mutate(service_frequency = ifelse(is.na(service_frequency), 0, service_frequency))

# Aggregate rows for each station
train_stop_count <- metro_train_stops %>%
  # Group by parent station
  group_by(parent_station) %>%
  # Sum the service frequency for each station
  summarise(
    service_frequency = sum(service_frequency),
  ) %>%
  # Rename variable for joining
  rename(stop_id = parent_station)
  
# Join stop frequencies to parent stations
train_stop_frequency <- left_join(train_parent_stations, train_stop_count, by = "stop_id")

###################################################################

# Read in metro tram data
metro_tram_stop_locations <- read.delim("data/gtfs/3/google_transit/stops.txt", header = TRUE, sep = ",")
metro_tram_stop_times <- read.delim("data/gtfs/3/google_transit/stop_times.txt", header = TRUE, sep = ",")
metro_tram_calendar_dates <- read.delim("data/gtfs/3/google_transit/calendar_dates.txt", header = TRUE, sep = ",")
metro_tram_calendar <- read.delim("data/gtfs/3/google_transit/calendar.txt", header = TRUE, sep = ",")
metro_tram_trips <- read.delim("data/gtfs/3/google_transit/trips.txt", header = TRUE, sep = ",")

tram_weekday_services <- metro_tram_calendar %>%
  filter(monday == 1 | tuesday == 1 | wednesday == 1 | thursday == 1 | friday == 1) %>%
  select(service_id)

tram_services <- semi_join(metro_tram_trips, tram_weekday_services, by = "service_id")

tram_stop_locations <- metro_tram_stop_locations %>%
  # Keep only rows where location is not a parent station
  filter(is.na(location_type)) %>%
  select(stop_id, stop_name, stop_lat, stop_lon)

tram_stop_frequencies <- semi_join(metro_tram_stop_times, tram_services, by = "trip_id")

tram_stop_frequencies <- tram_stop_frequencies %>%
  # Group by stop_id
  group_by(stop_id) %>%
  # Count number of services at given stop_id
  summarise(service_frequency = n_distinct(trip_id))

tram_stop_frequency <- left_join(tram_stop_locations, tram_stop_frequencies, by = "stop_id")

###################################################################

# Read in myki bus data
myki_bus_stop_locations <- read.delim("data/gtfs/4/google_transit/stops.txt", header = TRUE, sep = ",")
myki_bus_stop_times <- read.delim("data/gtfs/4/google_transit/stop_times.txt", header = TRUE, sep = ",")
myki_bus_calendar_dates <- read.delim("data/gtfs/4/google_transit/calendar_dates.txt", header = TRUE, sep = ",")
myki_bus_calendar <- read.delim("data/gtfs/4/google_transit/calendar.txt", header = TRUE, sep = ",")
myki_bus_trips <- read.delim("data/gtfs/4/google_transit/trips.txt", header = TRUE, sep = ",")

bus_weekday_services <- myki_bus_calendar %>%
  filter(monday == 1 | tuesday == 1 | wednesday == 1 | thursday == 1 | friday == 1) %>%
  select(service_id)

bus_services <- semi_join(myki_bus_trips, bus_weekday_services, by = "service_id")


bus_stop_locations <- myki_bus_stop_locations %>%
  # Keep only rows where location is not a parent station
  filter(is.na(location_type)) %>%
  # Keep only relevant columns
  select(stop_id, stop_name, stop_lat, stop_lon)

bus_stop_frequencies <- semi_join(myki_bus_stop_times, bus_services, by = "trip_id")

bus_stop_frequencies <- bus_stop_frequencies %>%
  # Group by stop_id
  group_by(stop_id) %>%
  # Count number of services at given stop_id
  summarise(service_frequency = n_distinct(trip_id))

# Join bus stop frequencies with bus stop locations
bus_stop_frequency <- left_join(bus_stop_locations, bus_stop_frequencies, by = "stop_id")

###################################################################

# Load readxl library
library(readxl)

# Read in population estimate data
aus_population_estimates <- read_excel("data/est_pop_sa2.xlsx", sheet = "Table 1", skip = 6)

metro_vic <- aus_population_estimates %>%
  rename(SA2_CODE_2021 = "SA2 code",
         GCCSA_name = "GCCSA name") %>%
  filter(GCCSA_name == "Greater Melbourne") %>%
  select(SA2_CODE_2021)

metro_vic_population_est <- aus_population_estimates %>%
  # Rename all variable names
  rename(GCCSA_name = "GCCSA name",
         SA2_CODE_2021 = "SA2 code",
         SA2_name = "SA2 name",
         "2001" = "no....11",
         "2024" = "no....34"
         ) %>% 
  # Filter for only SA2s in metropolitan Victoria
  filter(GCCSA_name == "Greater Melbourne") %>%
  mutate(absolute_change = `2024` - `2001`,
         percent_change = ifelse(`2001` <= 500, NA,
                                 round((absolute_change / `2001`) * 100, 3))) %>%
  # Only keep relevant variables
  select(SA2_CODE_2021, SA2_name, `2001`, `2024`, absolute_change, percent_change)


library(sf)

#  vic_sf <- sf::st_read("data/SA1_2021_AUST_SHP_GDA2020/SA1_2021_AUST_GDA2020.shp") %>%
#    st_simplify(dTolerance = 10000)
#  vic_geo <- sf::st_geometry(vic_sf)
#  plot(vic_geo)
  
#  vic <- ggplot(vic_sf) +
#    geom_sf(aes(geometry = geometry)) +
#    coord_sf(xlim = c(140, 150), ylim = c(-34, -39))

# vic_sf <- st_read("data/G33_VIC_GDA2020.gpkg")
geopath <- here::here("data/G33_VIC_GDA2020.gpkg")
st_layers(geopath)
vic_sa2 <- read_sf(geopath, layer = "G33_SA2_2021_VIC") %>%
  filter(SA2_NAME_2021 != "No usual address (Vic.)") %>%
  filter(SA2_NAME_2021 !="Migratory - Offshore - Shipping (Vic.)")

metro_vic$SA2_CODE_2021 <- as.character(metro_vic$SA2_CODE_2021)
metro_vic_sa2s <- left_join(metro_vic, vic_sa2, by = "SA2_CODE_2021")

# Convert back into sf object
metro_vic_sa2s <- st_as_sf(metro_vic_sa2s, crs = 7844)

bus_points <- st_as_sf(bus_stop_frequency, coords = c("stop_lon", "stop_lat"), crs = 7844)
bus_points <- st_transform(bus_points, st_crs(metro_vic_sa2s))
bus_points <- st_filter(bus_points, metro_vic_sa2s)
bus_points <- bus_points %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry()

bus_points <- bus_points %>%
  rename(stop_lon = "X",
         stop_lat = "Y")

bus_points <- as.data.frame(bus_points)

ggplot(metro_vic_sa2s) +
  geom_sf(aes(geometry = geom)) +
  geom_point(data = train_stop_frequency, aes(x = stop_lon, y = stop_lat), size = 0.1) +
  geom_point(data = tram_stop_frequency, aes(x = stop_lon, y = stop_lat), size = 0.1) +
  geom_point(data = bus_points, aes(x = stop_lon, y = stop_lat), size = 0.1)

tram_stop_frequency$stop_id <- as.character(tram_stop_frequency$stop_id)
bus_points$stop_id <- as.character(bus_points$stop_id)
stop_frequencies <- full_join(train_stop_frequency, tram_stop_frequency)
stop_frequencies <- full_join(stop_frequencies, bus_points)

ggplot(metro_vic_sa2s) +
  geom_sf(aes(geometry = geom)) +
  geom_point(data = stop_frequencies, aes(x = stop_lon, y = stop_lat, size = service_frequency), size = 0.1)

metro_vic_sa2s <- metro_vic_sa2s %>%
  select(SA2_CODE_2021, SA2_NAME_2021, geom)

stops_sf <- stop_frequencies %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 7844)

sa2 <- st_transform(metro_vic_sa2s, st_crs(stops_sf))

sa2 <- st_join(stops_sf, sa2)

sa2_service <- sa2 %>%
  st_drop_geometry() %>%
  group_by(SA2_CODE_2021, SA2_NAME_2021) %>%
  summarise(total_services = sum(service_frequency))

sa2_service <- sa2_service %>%
  left_join(metro_vic_sa2s)

ggplot(metro_vic_sa2s) +
  geom_sf(aes(geometry = geom)) +
  geom_sf(data = sa2_service, aes(geometry = geom, fill = total_services))

write_sf(sa2_service, "data/sa2_services.shp")
write.csv(stop_frequencies, "data/stop_frequencies.csv")
write.csv(metro_train_stops, "data/metro_train_stops.csv")
write.csv(bus_points, "data/myki_bus_stops.csv")
write.csv(metro_vic_population_est, "data/vic_popn.csv")
write.csv()