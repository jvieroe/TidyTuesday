library(tidyverse)
library(sf)
library(ggtext)
library(MetBrewer)
library(ggfx)

ming <- read_sf(dsn = "2022/week_01/data/Ming_Routes_2016",
                layer = "Ming_Routes_2016")

maps <- read_sf(dsn = "2022/week_01/data/ne_10m_admin_0_countries",
                layer = "ne_10m_admin_0_countries")

maps <- maps %>%  
  filter(SOVEREIGNT != "Antarctica")

latitude <- 32
longitude <- 90

ortho <- paste0('+proj=ortho +lat_0=', latitude, ' +lon_0=', longitude,
                ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')


maps_reproj <- maps %>% 
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON', do_split = TRUE) %>%
  st_transform(crs = ortho)

ming_reproj <- ming %>% 
  st_transform(crs = ortho)

china <- maps_reproj %>% 
  filter(SOVEREIGNT == "China")

st_bbox(china)

met.brewer("Greek", type = "discrete")
pal <- met.brewer("Greek", type = "discrete")

cntry_col <- pal[2]
cntry_fill <- pal[1]
roads_fill <- pal[5]

bkg_col <- "gray10"
fg_col <- pal[3]


ggplot() +
  geom_sf(data = maps_reproj, size = 0.2, fill = cntry_fill, color = cntry_col) +
  with_blur(
    geom_sf(data = ming_reproj, color = roads_fill, size = 2),
    sigma = unit(1, 'mm')
  ) +
  geom_sf(data = ming_reproj, color = roads_fill, size = .5) +
  annotate("richtext", x = 250000, y = 2100000,
           label = "<span style ='font-family:Cinzel'> The Ming Dynasty</span> 
           <br> —
           <span style='font-size:30px'>明朝</span> —
           <br> 
           <span style ='font-family:Cinzel;font-size:25px'>Trade Routes</span>",
           family = "Noto Sans TC",
           size = 9,
           label.color = roads_fill,
           text.color = roads_fill,
           fill = roads_fill, alpha = .3) +
  annotate("richtext", x = 250000, y = 2100000,
           label = "<span style ='font-family:Cinzel'> The Ming Dynasty</span> 
           <br> —
           <span style='font-size:30px'>明朝</span> —
           <br> 
           <span style ='font-family:Cinzel;font-size:25px'>Trade Routes</span>",
           family = "Noto Sans TC",
           size = 9,
           label.color = roads_fill,
           text.color = roads_fill,
           fill = NA, alpha = 1) +
  annotate("richtext", x = 3900000, y = -1700000,
           label = "Graphics: Jeppe Vierø (<span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> / <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b;</span> jvieroe)
           <br>
           Data: Berman & Zhang (2017)",
           family = "Cinzel",
           size = 3,
           angle = 90,
           hjust = 0,
           label.color = NA,
           text.color = roads_fill,
           fill = NA, alpha = 1) +
  coord_sf(xlim = c(-1200000, 3800000),
           ylim = c(-1500000, 2600000)) +
  theme_void() +
  theme(panel.background = element_rect(fill = bkg_col, color = bkg_col),
        plot.background = element_rect(fill = bkg_col, color = bkg_col),
        panel.grid.major = element_line(color = fg_col, size = .1),
        plot.margin = grid::unit(c(t = 0, r = 0, b = 0, l = 0), "mm"))


ggsave(plot = last_plot(), "2022/week_01/ming.png",
       dpi = 600,
       width = 8.5, height = 7)

