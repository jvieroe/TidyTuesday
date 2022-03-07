# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html

library(tidyverse)
library(ggtext)
library(janitor)
library(sf)
library(rnaturalearth)
library(viridis)

tmap_mode("view")

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations <- stations %>% 
  clean_names()

stations <- stations %>% 
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>% 
  select(access_code)

usa <- rnaturalearth::ne_states("United States of America") %>% 
  st_as_sf()

alaska <- usa %>% 
  filter(name == "Alaska")

hawaii <- usa %>% 
  filter(name == "Hwaii")

usa <- usa %>% 
  filter(!name %in% c("Alaska", "Hawaii"))

# usa <- usa %>% 
#   st_transform(crs = 3857)

tm_shape(usa) +
  tm_polygons()

usa_union <- usa %>% 
  summarize()

usa_union <- usa_union %>% 
  st_transform(3857)

stations <- stations %>% 
  st_transform(3857)

st_is_longlat(usa_union)

us_grid <- usa_union %>% 
  st_make_grid(.,
               cellsize = c(50*10^3,
                            50*10^3),
               square = FALSE) %>% 
  st_as_sf()

us_grid <- us_grid %>% 
  st_intersection(usa_union)


stations <- stations %>% 
  mutate(us_dist = st_distance(.,
                               usa_union))

stations <- stations %>% 
  mutate(us_dist = unclass(us_dist))

stations <- stations %>% 
  filter(us_dist < 50*10^3)

us_grid <- us_grid %>% 
  mutate(grid_id = row_number())

intersections_data <- stations %>% 
  st_join(.,
          us_grid,
          join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  select(access_code,
         grid_id)


intersections_data <- intersections_data %>% 
  group_by(access_code,
           grid_id,
           .drop = FALSE) %>% 
  tally()

intersections_data <- intersections_data %>% 
  pivot_wider(id_cols = grid_id,
              values_from = n,
              names_from = access_code)


us_grid <- us_grid %>% 
  tidylog::left_join(.,
                     intersections_data,
                     by = "grid_id")

us_grid <- us_grid %>% 
  mutate(across(c(public, private),
                ~ ifelse(is.na(.x),
                         0,
                         .x)))

ggplot() +
  geom_sf(data = us_grid, aes(fill = private)) +
  scale_fill_viridis(direction = -1,
                     option = "F",
                     name = "Share")


tm_shape(stations) +
  tm_dots()
