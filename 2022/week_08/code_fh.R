library(tidyverse)
library(janitor)
library(viridis)
library(ggtext)

rm(list = ls())

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- df %>% 
  clean_names()

plot_df <- df %>% 
  filter(!is.na(region_name)) %>% 
  filter(year == 2020)

plot_df <- plot_df %>% 
  mutate(across(c(cl, pr, region_name),
                ~ factor(.x)))

df_tile <- plot_df %>% 
  group_by(region_name,
           .drop = FALSE) %>% 
  mutate(n_cntry = n()) %>% 
  ungroup() %>% 
  arrange(region_name)

df_tile <- df_tile %>% 
  group_by(cl, pr, region_name, n_cntry,
           .drop = FALSE) %>% 
  summarize(n_group = n()) %>% 
  ungroup() 

df_tile <- df_tile %>%
  mutate(n_cntry = as.numeric(n_cntry))

df_tile <- df_tile %>% 
  arrange(region_name, cl, pr)

df_tile <- df_tile %>% 
  group_by(region_name) %>% 
  mutate(n_cntry = mean(n_cntry, na.rm = TRUE)) %>% 
  ungroup()

df_tile <- df_tile %>% 
  mutate(share = n_group / n_cntry) %>% 
  group_by(region_name) %>% 
  mutate(vali = sum(share)) %>% 
  ungroup()

tile_line <- "white"
strip_col <- "transparent"
strip_fill <- "white"
bckgrnd_col <- "white"

ggplot(df_tile, aes(x = pr, y = cl, fill = share)) +
  geom_tile(color = tile_line,
            size = .75) +
  coord_fixed() +
  scale_fill_viridis(direction = -1,
                     option = "F",
                     name = "Share",
                     breaks = seq(0, 0.5, 0.25),
                     labels = scales::percent) +
  labs(x = "Political Rights",
       y = "Civil Liberties",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 8 | Data: Freedom House",
       title = "State of Freedom 2020",
       subtitle = "<subtitle text>") +
  facet_wrap(~ region_name) +
  theme(axis.ticks = element_blank(),
        strip.background = element_rect(fill = strip_fill,
                                        color = strip_col),
        legend.position = c(0.85, 0.25),
        legend.direction = "horizontal",
        # panel.background = element_rect(fill = bckgrnd_col,
        #                                 color = bckgrnd_col),
        plot.background = element_rect(fill = bckgrnd_col,
                                       color = bckgrnd_col),
        legend.background = element_rect(fill = bckgrnd_col,
                                         color = bckgrnd_col),
        plot.caption = ggtext::element_markdown(hjust = 0,
                                                margin = ggplot2::margin(t = 20, 
                                                                         unit = "pt")),
        plot.caption.position = "plot") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               ticks = FALSE,
                               barwidth = 9,
                               barheight = 0.8))

ggsave(plot = last_plot(),
       filename = "2022/week_08/fh.png")
