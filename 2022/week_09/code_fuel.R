rm(list=ls())

# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html

library(tidyverse)
library(ggtext)
library(janitor)
library(sf)
library(rnaturalearth)
library(viridis)
library(tmap)

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
  filter(name == "Hawaii")

usa <- usa %>% 
  filter(!name %in% c("Alaska", "Hawaii"))

usa <- usa %>%
  st_transform(crs = 3857)

# usa <- usa %>%
#   st_simplify(dTolerance = 20000)


usa_union <- usa %>%
  summarize()


usa_union <- usa_union %>%
  st_transform(3857)

stations <- stations %>% 
  st_transform(3857)

latitude <- 38
longitude <- -100

ortho <- paste0('+proj=ortho +lat_0=', latitude, ' +lon_0=', longitude,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


usa <- usa %>% 
  st_transform(ortho)


usa_union <- usa_union %>% 
  st_transform(ortho)

stations <- stations %>% 
  st_transform(ortho)


# usa_union <- usa_union %>%
#   st_simplify(dTolerance = 10000)

us_grid <- usa_union %>% 
  summarise() %>% 
  st_make_grid(.,
               cellsize = c(200*10^3,
                            200*10^3),
               square = FALSE) %>% 
  st_as_sf()

# us_grid <- us_grid %>% 
#   st_intersection(usa_union)

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


# us_grid <- us_grid %>% 
#   mutate(across(c(public, private),
#                 ~ ifelse(is.na(.x),
#                          0,
#                          .x)))

log(NA)

us_grid <- us_grid %>% 
  mutate(across(c(public, private),
                ~ log(.x + 1),
                .names = "ln_{.col}"))

us_grid <- us_grid %>%
  mutate(across(ends_with(c("public", "private")),
                ~ ifelse(is.na(.x),
                         0,
                         .x)))



us_plot <- usa %>% 
  st_intersection(usa_union) %>% 
  st_simplify(dTolerance = 5000)

us_grid <- us_grid %>% 
  st_intersection(us_plot)

bkg_col <- "gray20"
bkg <- element_rect(fill = bkg_col,
                    color = bkg_col)
txt_col <- "white"


ggplot() +
  geom_sf(data = us_grid, aes(fill = ln_public),
          color = "white",
          size = 0.05) +
  geom_sf(data = us_plot,
          fill = NA,
          color = "white",
          size = 0.35) +
  scale_fill_viridis(direction = -1,
                     option = "F",
                     name = "Share") +
  theme_void() +
  labs(title = "Public Access to Alternative Fuel in the US",
       subtitle = "yy",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 9 | Data: US DOT") +
  coord_sf(clip = "off") +
  theme(panel.background = bkg,
        plot.background = bkg,
        plot.title = ggtext::element_markdown(hjust = 0.5,
                                              color = txt_col),
        plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                 color = txt_col),
        plot.caption = ggtext::element_markdown(hjust = 0.5,
                                                color = txt_col),
        panel.grid.major = element_line(color = "white",
                                        size = .06),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = 0,
                                      unit = "pt"))

ggsave(plot = last_plot(),
       filename = "2022/week_09/fuel.png")

