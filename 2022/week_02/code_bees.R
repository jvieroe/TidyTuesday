library(tidyverse)
library(tidytuesdayR)
library(sf)

tt <- tidytuesdayR::tt_load('2022-01-11')

colony <- tt[[1]]

stressor <- tt[[2]]

rm(tt)



hexa <- read_sf(dsn = "2022/week_02/data/us_states_hexgrid",
                layer = "us_states_hexgrid")

hexa <- hexa %>% 
  st_transform(crs = 3857)


ggplot() +
  geom_sf(data = hex)

hexa <- hexa %>% 
  mutate(state = gsub(" \\(United States\\)", "", google_nam)) %>% 
  relocate(state, .before = geometry)


hexa <- hexa %>% 
  mutate(cent = st_centroid(.))

cents <- hexa %>% 
  st_centroid()

ggplot() +
  geom_sf(data = hex) +
  geom_sf(data = cents)



class(hexa$geometry)



