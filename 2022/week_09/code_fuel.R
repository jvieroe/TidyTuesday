library(tidyverse)
library(ggtext)
library(janitor)
library(sf)
library(rnaturalearth)
library(viridis)
library(MetBrewer)

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations <- stations %>% 
  clean_names()

stations <- stations %>% 
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>% 
  select(access_code)

usa <- rnaturalearth::ne_states("United States of America") %>% 
  st_as_sf()

usa <- usa %>% 
  filter(!name %in% c("Alaska", "Hawaii"))

usa <- usa %>%
  st_transform(crs = 3857)

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

us_grid <- usa_union %>% 
  summarise() %>% 
  st_make_grid(.,
               cellsize = c(200*10^3,
                            200*10^3),
               square = FALSE) %>% 
  st_as_sf()

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

txt_font <- "Fira Sans"


pal <- met.brewer("VanGogh3",
                  type = "continuous")

scales::show_col(pal)

pal_text <- pal %>% unlist() %>% as.character()
pt1 <- pal_text[1]
txt_col <- pal_text[2]
pt8 <- pal_text[8]

pal_text[5]

ggplot() +
  geom_sf(data = us_grid, aes(fill = ln_public),
          color = "white",
          size = 0.05) +
  geom_sf(data = us_plot,
          fill = NA,
          color = "white",
          size = 0.225) +
  scale_fill_gradientn(colors = pal) +
  theme_void() +
  labs(title = "Alternative Fuel Stations in the US",
       subtitle = "Number (log) of Alternative Fuel Stations with public access.<br><span style='color:#669D62'>Darker colors </span> reflect a larger number of stations",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 9 | Data: US DOT") +
  coord_sf(clip = "off",
           xlim = c(st_bbox(us_grid)[[1]]*1.00,
                    st_bbox(us_grid)[[3]]*1.00),
           ylim = c(st_bbox(us_grid)[[2]]*1.122,
                    st_bbox(us_grid)[[4]]*1.122)) +
  theme(panel.background = bkg,
        plot.background = bkg,
        plot.title = ggtext::element_markdown(hjust = 0.5,
                                              color = pt1,
                                              size = 28,
                                              family = txt_font,
                                              margin = ggplot2::margin(t = 10,
                                                                       b = -30,
                                                                       unit = "pt")),
        plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                 color = txt_col,
                                                 size = 14,
                                                 family = txt_font,
                                                 margin = ggplot2::margin(t = 35,
                                                                          b = -35,
                                                                          unit = "pt")),
        plot.caption = ggtext::element_markdown(hjust = 0.5,
                                                color = txt_col,
                                                family = txt_font,
                                                margin = ggplot2::margin(t = -10,
                                                                         unit = "pt")),
        panel.grid.major = element_line(color = "white",
                                        size = .06),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = 0,
                                      unit = "pt"))

ggsave(plot = last_plot(),
       filename = "2022/week_09/fuel.png")

