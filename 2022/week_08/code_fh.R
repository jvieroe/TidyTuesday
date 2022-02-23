library(tidyverse)
library(janitor)
library(viridis)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- df %>% 
  clean_names()

df

df %>% 
  filter(is.na(cl) | is.na(pr)) %>% 
  tally()

exp_val <- 0.2

ggplot(df, aes(x = pr, y = cl, color = region_name, fill = region_name)) +
  geom_point(shape = 21,
             position = position_jitter(width = 0.5,
                                        height = 0.5),
             alpha = .25) +
  geom_smooth(method = "lm") +
  # scale_x_continuous(limits = c(1, 7),
  #                    expand = c(exp_val, exp_val)) +
  # scale_y_continuous(limits = c(1, 7),
  #                    expand = c(exp_val, exp_val)) +
  theme(legend.position = "none") +
  facet_wrap(~ region_name)

min(df$cl)
max(df$cl)

min(df$pr)
max(df$pr)



euro <- df %>% 
  #filter(region_name == "Europe") %>% 
  filter(year == 2020)

df_tile <- euro %>% 
  mutate(across(c(cl, pr),
                ~ factor(.x)))

df_tile <- df_tile %>% 
  mutate(n_cntry = n()) %>% 
  group_by(cl, pr, n_cntry,
           .drop = FALSE) %>% 
  summarize(n_group = n()) %>% 
  ungroup()

df_tile <- df_tile %>% 
  mutate(n_cntry = mean(n_cntry, na.rm = TRUE)) %>% 
  mutate(share = n_group / n_cntry)


tile_line <- "white"

ggplot(df_tile, aes(x = pr, y = cl, fill = share)) +
  geom_tile(color = tile_line,
            size = .75) +
  coord_fixed()

# ------------------------

plot_df <- df %>% 
  filter(!is.na(region_name)) %>% 
  filter(year == 2020)

plot_df <- plot_df %>% 
  mutate(across(c(cl, pr),
                ~ factor(.x)))

df_tile <- plot_df %>% 
  group_by(region_name) %>% 
  mutate(n_cntry = n()) %>% 
  ungroup()

df_tile <- df_tile %>% 
  group_by(cl, pr, region_name, n_cntry,
           .drop = FALSE) %>% 
  summarize(n_group = n()) %>% 
  ungroup()

df_tile <- df_tile %>% 
  arrange(region_name, pr, cl)

df_tile <- df_tile %>% 
  group_by(region_name) %>% 
  mutate(n_cntry = mean(n_cntry, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(share = n_group / n_cntry)


tile_line <- "white"

ggplot(df_tile, aes(x = pr, y = cl, fill = share)) +
  geom_tile(color = tile_line,
            size = .75) +
  coord_fixed()




# ------------------------

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
  #mutate(n_cntry = factor(n_cntry)) %>%
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
                                         color = bckgrnd_col)) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               ticks = FALSE,
                               barwidth = 9,
                               barheight = 0.8))

ggsave(plot = last_plot(),
       filename = "2022/week_08/fh.png")