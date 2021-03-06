library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(sf)
library(janitor)
library(cowplot)
library(colorspace)
library(shadowtext)
library(ggtext)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md


# ------------------------------------------------------------------
# TidyTuesday data
# ------------------------------------------------------------------
tt <- tidytuesdayR::tt_load('2022-01-11')
colony <- tt[[1]]
stressor <- tt[[2]]

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
  summarize(lost_pct = mean(colony_lost_pct, na.rm = TRUE),
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

hexa <- hexa %>% 
  mutate(fill = ifelse(is.na(fill), "transparent", fill))

bi_legend <- col_scale %>% 
  separate(group, into = c("stressor_pct", "lost_pct"), sep = " - ") %>%
  mutate(stressor = as.integer(stressor_pct),
         lost_pct = as.integer(lost_pct))



# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------

# ----- Plot map
font1 <- "Licorice"
font2 <- "Roboto"

bgk_col <- "#C4952E"
fg_col <- colorspace::darken("#93500C", 0.2)


# ----- Plot legend
legend <- ggplot() +
  geom_tile(data = bi_legend,
            mapping = aes(x = lost_pct,
                          y = stressor_pct,
                          fill = fill)) +
  scale_fill_identity() +
  labs(x = expression("More colonies affected" %->%""),
       y = expression("More colonies lost" %->%"")) +
  cowplot::theme_map() +
  theme(axis.title.x = element_text(size = 7,
                                    family = font2,
                                    color = fg_col),
        axis.title.y = element_text(size = 7,
                                    family = font2,
                                    color = fg_col,
                                    angle = 90)) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"))


map <- ggplot() +
  geom_sf(data = hexa, aes(fill = fill), color = "white", size = .65) +
  geom_shadowtext(data = cents, aes(x = X, y = Y, label = label)) +
  scale_fill_identity() +
  labs(title = "",
       subtitle = "",
       caption = "") + 
  theme_void() +
  theme(panel.background = element_rect(fill = bgk_col,
                                        color = bgk_col),
        plot.background = element_rect(fill = bgk_col,
                                       color = bgk_col))

title <- ggplot() +
  annotate("text", x = 1, y = 1,
           label = "Bee Colonies under stress",
           family = font1,
           size = 22,
           color = fg_col) +
  theme_void()

subtitle <- ggplot() +
  annotate("text", x = 1, y = 1,
           label = "Share of colonies affected by the Varroa mite and share of colonies lost,\nmeasured at the state-quarter level and averaged across the period 2015-2021",
           family = font2,
           size = 4,
           color = fg_col) +
  theme_void()

caption <- ggplot() +
  annotate("richtext", x = 1, y = 1,
           label = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 2",
           family = font2,
           size = 3,
           label.color = NA,
           text.color = fg_col,
           fill = NA, alpha = 1) +
  theme_void()



# ----- Combine plots
p <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, 0.01, 0.22, 0.22) +
  draw_plot(title, 0, .41, 1, 1) +
  draw_plot(subtitle, 0, .31, 1, 1) +
  draw_plot(caption, -0.25, -0.465, 1, 1) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(plot = p,
       "2022/week_02/bees.png",
       dpi = 600,
       width = 9, height = 6)

knitr::plot_crop("2022/week_02/bees.png")


