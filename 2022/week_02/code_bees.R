library(tidyverse)
library(tidytuesdayR)
library(sf)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md


tt <- tidytuesdayR::tt_load('2022-01-11')

colony <- tt[[1]]

stressor <- tt[[2]]

rm(tt)



hexa <- read_sf(dsn = "2022/week_02/data/us_states_hexgrid",
                layer = "us_states_hexgrid")

hexa <- hexa %>% 
  st_transform(crs = 3857)


ggplot() +
  geom_sf(data = hexa)

hexa <- hexa %>% 
  mutate(state = gsub(" \\(United States\\)", "", google_nam)) %>% 
  relocate(state, .before = geometry)


hexa <- hexa %>% 
  mutate(cent = st_centroid(.))

cents <- hexa %>% 
  st_centroid()

ggplot() +
  geom_sf(data = hexa) +
  geom_sf(data = cents)




colony <- colony %>% 
  filter(year %in% c(2015, 2021)) %>% 
  filter((year == 2015 & months == "January-March") | (year == 2021 & months == "April-June"))




