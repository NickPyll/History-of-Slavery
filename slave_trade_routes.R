# Get the Data

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

library(pdftools) # reading in the PDF tables
library(tidyverse) # requires tidy 1.0 and dplyr 1.0 for below example
library(cfeaR)
library(magrittr)
library(scales)
library(maps)
library(ggmap)
library(cfeaR)
library(gganimate)

census %<>%
  mutate(slave_pct = black_slaves / black)

census %>%
  filter(region != 'USA Total') %>%
  group_by(region, year) %>%
  summarize(black_slaves = sum(black_slaves)) %>%
  ggplot(aes(x = year, y = black_slaves, color = region)) +
  geom_line() +
  theme_cfeaR() +
  scale_color_manual(values = group_col_cfeaR(n = 9), name = 'Region') +
  labs(title = 'Slave Population by Decade',
       subtitle = 'US Regions',
       y = 'Number of Slaves Recorded in Census',
       x = 'Year') +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1790, 1870, by = 10))

# cities <-
#   slave_routes %>%
#   rename(place = port_origin) %>%
#   select(place) %>%
#   full_join(slave_routes %>%
#               rename(place = port_arrival) %>%
#               select(place),
#             by = 'place') %>%
#   distinct() %>%
#   filter(!is.na(place)) %>%
#   filter(place != '???') %>%
#   mutate(place_clean = str_remove(place, ', port unspecified')) %>%
#   mutate(place_clean = str_remove(place_clean, ', colony unspecified'))
# 
# locations_df <- mutate_geocode(cities, place_clean)
# 
# # some locations manually geocoded
# locations_manual <- 
#   tribble(
#     ~place_clean, ~lat2, ~lon2,
#     'Liverpool', 53.616958, -2.964553,
#     'Exeter', 50.711446, -3.517359,
#     'Oporto', 41.149730, -8.628477,
#     'British Leewards', 17.036381, -61.800424,
#     'Kingston', 17.982607, -76.807978,
#     'British Honduras', 15.787378, -86.675558,
#     'Canasi', 23.140905, -81.779888,
#     'Shoreham', 40.955518, -72.859002,
#     'Lyme', 50.723333, -2.935192,
#     'Preston', 53.763533, -2.804872,
#     'Portuguese Guinea', 11.843791, -15.734914,
#     'Rappahannock', 38.289725, -77.021941,
#     'Cumingsberg', 6.824933, -58.299845,
#     'Mariel', 23.003344, -82.746320,
#     'Ilha de Lobes', 21.457687, -97.359273,
#     'Dutch Guianas', 5.906715, -55.350919,
#     'Senegambia and offshore Atlantic', 13.461769, -16.677456,
#     'Annotto Bay', 18.272593, -76.782669
#   )
# 
# locations_clean <- 
#   locations_df %>%
#   rename(lat1 = lat,
#          lon1 = lon) %>%
#   left_join(locations_manual,
#             by = 'place_clean') %>%
#   mutate(lat = coalesce(lat2, lat1),
#          lon = coalesce(lon2, lon1)) %>%
#   select(place, lat, lon)

# saveRDS(locations_clean, 'locations_clean.rds')

locations_clean <- readRDS('locations_clean.rds')

locations_clean %>%
  distinct()

slave_route_agg <-
  slave_routes %>%
  group_by(port_origin, port_arrival) %>%
  summarize(n_slaves = sum(n_slaves_arrived)) %>%
  ungroup() %>%
  filter(!is.na(port_origin)) %>%
  filter(!is.na(port_arrival)) %>%
  filter(!is.na(n_slaves)) %>%
  left_join(locations_clean %>%
              rename(port_origin = place,
                     flat = lat,
                     flon = lon),
            by = 'port_origin') %>%
  left_join(locations_clean %>%
              rename(port_arrival = place,
                     tlat = lat,
                     tlon = lon),
            by = 'port_arrival') %>%
  filter(port_arrival != '???') %>%
  filter(port_origin != port_arrival)

mapWorld <- borders("world", colour = "gray70", fill = "gray95") # create a layer of borders

ggplot() + 
  mapWorld +
  coord_sf(xlim = c(-150, 50), ylim = c(-50, 75), expand = FALSE) +
  # coord_sf(xlim = c(-85, -75), ylim = c(30, 40), expand = FALSE) +
  geom_curve(data = slave_route_agg %>%
               rename('Number of Slaves' = n_slaves),
             aes(x = flon, y = flat, xend = tlon, yend = tlat, size = `Number of Slaves`, alpha = `Number of Slaves`),
             col = "#E31A1C",
             # size = .05,
             curvature = 0.2) +
  theme_cfeaR() +
  labs(title = 'Slave Trade Routes',
       # subtitle = 'US Regions',
       y = 'Latitude',
       x = 'Longitude') +
  scale_size(range = c(.2, .7))  
  
slave_route_year <-
  slave_routes %>%
  group_by(port_origin, port_arrival, year_arrival) %>%
  summarize(n_slaves = sum(n_slaves_arrived)) %>%
  ungroup() %>%
  filter(!is.na(port_origin)) %>%
  filter(!is.na(port_arrival)) %>%
  filter(!is.na(n_slaves)) %>%
  left_join(locations_clean %>%
              rename(port_origin = place,
                     flat = lat,
                     flon = lon),
            by = 'port_origin') %>%
  left_join(locations_clean %>%
              rename(port_arrival = place,
                     tlat = lat,
                     tlon = lon),
            by = 'port_arrival') %>%
  filter(port_arrival != '???') %>%
  filter(port_origin != port_arrival) 
  # manually add a row to solve animation issue at beginning...this didn't work as intended
  # add_row(port_origin = 'Lisbon',
  #         port_arrival = 'Spanish Caribbean, unspecified',
  #         year_arrival = 1561,
  #         n_slaves = 1, 
  #         flat = 38.72225, flon = -9.1393366, tlat = 21.4691137, tlon =	-78.656894)

p <-
  ggplot() + 
  mapWorld +
  coord_sf(xlim = c(-150, 50), ylim = c(-50, 75), expand = FALSE) +
  geom_curve(data = slave_route_year %>%
               filter(year_arrival > 1532) %>%
               rename('Number of Slaves' = n_slaves),
             aes(x = flon, y = flat, xend = tlon, yend = tlat, size = `Number of Slaves`, alpha = `Number of Slaves`),
             col = "#E31A1C",
             # size = .05,
             curvature = 0.2) +
  theme_cfeaR() +
  labs(title = 'Slave Trade Routes',
       # subtitle = 'US Regions',
       y = 'Latitude',
       x = 'Longitude') +
  scale_size(range = c(.2, 1)) +
  transition_time(as.integer(year_arrival)) +
  labs(title = 'ATLANTIC SLAVE TRADE 1562 - 1864',
       subtitle = "Year: {frame_time}") +
  shadow_mark(alpha = 0.3, size = 0.05) 

animate(p, fps = 5, height = 6, width = 9, units = 'in', res = 150)

anim_save("slave_trade_routes.gif")

