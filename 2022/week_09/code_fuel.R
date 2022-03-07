# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html

library(tidyverse)
library(ggtext)
library(janitor)
library(sf)
library(rnaturalearth)

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
               cellsize = c(100*10^3,
                            100*10^3),
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

intersections_data <- stations %>% 
  mutate(grid_id = st_join(.,
                           us_grid,
                           join = st_nearest_feature))

intersections_data <- stations %>% 
  mutate(grid_id = st_intersects(.,
                                 us_grid)) %>% 
  mutate(grid_id = as.numeric(grid_id)) %>% 
  #st_drop_geometry() %>% 
  select(access_code,
         grid_id)

any(is.na(intersections_data$grid_id))

test <- intersections_data %>% 
  filter(is.na(grid_id))

tm_shape(test) +
  tm_dots()


temp1 <- temp1 %>% 
  group_by()

class(temp1$grid_id)




