library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(sf)
library(janitor)
library(ggsflabel)
library(cowplot)
library(colorspace)
library(shadowtext)
library(ggtext)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md


# ------------------------------------------------------------------
# TidyTuesday data
# ------------------------------------------------------------------
# tt <- tidytuesdayR::tt_load('2022-01-11')
# 
# colony <- tt[[1]]
# stressor <- tt[[2]]

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

colony <- colony %>% 
  filter(!state %in% c("United States", "Other States"))

stressor <- stressor %>% 
  filter(!state %in% c("United States", "Other States"))


stressor <- stressor %>% 
  pivot_wider(id_cols = c(year, months, state),
              names_from = stressor,
              values_from = stress_pct)


df <- colony %>% 
  tidylog::left_join(.,
                     stressor,
                     by = c("state", "year", "months")) %>% 
  clean_names()


df <- df %>% 
  group_by(state) %>% 
  mutate(lost_pct = mean(colony_lost_pct, na.rm = TRUE),
         stressor_pct = mean(varroa_mites, na.rm = TRUE)) %>% 
  ungroup()



# ------------------------------------------------------------------
# US Hex data
# ------------------------------------------------------------------
hexa <- read_sf(dsn = "2022/week_02/data/us_states_hexgrid",
                layer = "us_states_hexgrid")

hexa <- hexa %>% 
  st_transform(crs = 3857)

hexa <- hexa %>% 
  mutate(state = gsub(" \\(United States\\)", "", google_nam)) %>% 
  relocate(state, .before = geometry)


cents <- hexa %>% 
  st_centroid() %>% 
  select(state, iso3166_2)

cents <- cents %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  tibble() %>% 
  mutate(label = cents$iso3166_2)


# ------------------------------------------------------------------
# Merge data
# ------------------------------------------------------------------
setdiff(unique(df$state), unique(hexa$state))

df <- df %>% 
  distinct(., state,
           .keep_all = TRUE)

hexa <- hexa %>% 
  tidylog::left_join(.,
                     df,
                     by = "state")

rm(colony, stressor, df)

hexa <- hexa %>% 
  select(state, lost_pct, stressor_pct)


# ------------------------------------------------------------------
# Prepare bivariate color mapping
# ------------------------------------------------------------------
col_scale <- tibble("3 - 3" = "#3F2949",
                    "2 - 3" = "#435786",
                    "1 - 3" = "#4885C1",
                    "3 - 2" = "#77324C",
                    "2 - 2" = "#806A8A",
                    "1 - 2" = "#89A1C8",
                    "3 - 1" = "#AE3A4E",
                    "2 - 1" = "#BC7C8F",
                    "1 - 1" = "#CABED0") %>% 
  pivot_longer(cols = everything(),
               names_to = "group",
               values_to = "fill")

quantiles_x <- hexa %>%
  pull(stressor_pct) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

quantiles_y <- hexa %>%
  pull(lost_pct) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

hexa <- hexa %>% 
  mutate(quant_x = cut(stressor_pct,
                       breaks = quantiles_x,
                       include.lowest = TRUE),
         quant_y = cut(lost_pct,
                       breaks = quantiles_y,
                       include.lowest = TRUE)) %>% 
  mutate(group = paste(as.numeric(quant_x),
                       as.numeric(quant_y),
                       sep = " - ")) %>% 
  left_join(col_scale, by = "group")

na_col <- "transparent"

hexa <- hexa %>% 
  mutate(fill = ifelse(is.na(fill), na_col, fill))


bi_legend <- col_scale %>% 
  separate(group, into = c("stressor_pct", "lost_pct"), sep = " - ") %>%
  mutate(stressor = as.integer(stressor_pct),
         lost_pct = as.integer(lost_pct))



# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------

# ----- Plot map
honey_pal <- c("#E3D7C1", "#C8B188", "#C4952E", "#BE7C22", "#93500C")
bgk_col <- "goldenrod"
bgk_col <- honey_pal[1]
bgk_col <- colorspace::lighten("goldenrod", .05)


# ggplot() +
#   geom_sf(data = hexa, fill = honey_pal[5])


font <- "Pacifico"

map <- ggplot() +
  geom_sf(data = hexa, aes(fill = fill), color = "white", size = .5) +
  #geom_sf_text(data = cents, aes(label = iso3166_2), color = "white") +
  geom_shadowtext(data = cents, aes(x = X, y = Y, label = label)) +
  scale_fill_identity() +
  labs(title = "Bee colony loss") + 
  theme_void() +
  theme(panel.background = element_rect(fill = bgk_col,
                                        color = bgk_col),
        plot.background = element_rect(fill = bgk_col,
                                       color = bgk_col),
        plot.margin = margin(0, 0, 80, 0),
        plot.title = element_text(color = "black", family = font, size = 30,
                                  hjust = 0.5))


# ----- Plot legend
legend <- ggplot() +
  geom_tile(data = bi_legend,
            mapping = aes(x = lost_pct,
                          y = stressor_pct,
                          fill = fill)) +
  scale_fill_identity() +
  labs(x = expression("Higher stressor" %->%""),
       y = expression("More colonies lost" %->%"")) +
  cowplot::theme_map() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8,
                                    angle = 90)) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"))


# ----- Combine plots
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.775, 0.055, 0.2275, 0.2275)


