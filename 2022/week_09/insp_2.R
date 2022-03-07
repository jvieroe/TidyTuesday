library(extrafont)
library(extrafontdb)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)

## getting station data and merging

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') %>%
  janitor::clean_names() %>%
  filter(fuel_type_code == "ELEC") %>%
  left_join(read_csv("state_pops.csv") %>%
              rename("state" = "STATECODE",
                     "state_name" = "NAME")) %>%
  select(state,
         POPESTIMATE2019,
         state_name) %>%
  janitor::clean_names() %>%
  group_by(state,
           state_name) %>%
  summarise(stations = n(),
            population = mean(popestimate2019))

## getting map data

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2)) %>%
  filter(!id %in% c("HI", "AK"))

spdf_fortified <- tidy(spdf, region = "google_name")

merge <- spdf@data %>%
  select(google_name, iso3166_2)

## plotting time

plot <- stations %>%
  right_join(spdf_fortified, by = c("state_name" = "id")) %>%
  filter(!state_name %in% c("United States", "Other States", "Alaska", "Hawaii")) %>%
  left_join(merge, by = c("state_name" = "google_name")) %>%
  left_join(centers, by = c("iso3166_2" = "id")) %>%
  mutate(stations_per_100k = stations / population * 100000) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = stations_per_100k), colour = "#F8F5E6", size = 2) +
  theme_void() +
  coord_map() +
  geom_text(aes(x = x,
                y = y,
                label = ifelse(
                  is.na(stations_per_100k),
                  paste0(iso3166_2, "\n", NA),
                  paste0(iso3166_2, "\n", round(stations_per_100k, 0)))),
            color="grey10",
            size = 5,
            family = "Century Gothic") +
  scale_fill_gradient2(low = "#7F8B52", high = "#558776") +
  theme(plot.title = element_text(size = 32, family = "Century Gothic", face = "bold"),
        plot.background = element_rect(fill = "#F8F5E6", colour = "#F8F5E6"),
        plot.subtitle = element_text(size = 16, family = "Century Gothic"),
        legend.position = "none",
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(size = 10, family= "Century Gothic")) +
  labs(title = "\n\nVermont leading the charge for Electric Vehicle charging",
       subtitle = "\nNumber of stations with Electric Vehicle charging points per 100k population",
       caption = "Visualisation | Henry Wakefield\nTwitter | @henrywrover2\n Data | US DoT\nUS Census Bureau\nSource | https://data-usdot.opendata.arcgis.com/datasets/usdot::alternative-fueling-stations/about\nhttps://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html\n\n")

ggsave(plot, filename = "elec_stations.png", width = 20, height = 15)