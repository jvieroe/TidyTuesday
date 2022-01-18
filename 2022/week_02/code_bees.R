library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(sf)
library(janitor)
library(ggsflabel)
library(cowplot)
library(colorspace)
library(shadowtext)
library(colorjam)

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
  filter(state != "United States")


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

ggplot(df, aes(x = stressor_pct, y = lost_pct)) +
  geom_point()



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


# ------------------------------------------------------------------
# Merge data
# ------------------------------------------------------------------
vec1 <- unique(df$state)
vec2 <- unique(hexa$state)

intersect(vec1, vec2)
setdiff(vec1, vec2)

rm(vec1, vec2)

df <- df %>% 
  distinct(., state,
           .keep_all = TRUE)

hexa <- hexa %>% 
  tidylog::left_join(.,
                     df,
                     by = "state")



cents_raw <- cents %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  tibble() %>% 
  mutate(label = cents$iso3166_2)

ggplot() +
  geom_sf(data = hexa, color = "white", size = .5) +
  #geom_sf_text(data = cents, aes(label = iso3166_2), color = "black", size = 4) +
  geom_shadowtext(data = cents_raw, aes(x = X, y = Y, label = label))
  
# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------
# create 3 buckets for gini
quantiles_lost <- hexa %>%
  pull(lost_pct) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

quantiles_stressor <- hexa %>%
  pull(stressor_pct) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)


# h1 <- "#F2B104"
# h2 <- "#F59E02"
# h3 <- "#F79101"
# 
# b1 <- "#D7DCEA"
# b2 <- "#A1B3D7"
# b3 <- "#6581BF"
# 
# 
# bivariate_color_scale <- tibble("3 - 3" = "#AE8960",
#                                 "2 - 3" = "#AD9061",
#                                 "1 - 3" = "#AC9962",
#                                 "3 - 2" = "#CCA26C",
#                                 "2 - 2" = "#CBA96D",
#                                 "1 - 2" = "#CAB26E",
#                                 "3 - 1" = "#E7B776",
#                                 "2 - 1" = "#E6BD76",
#                                 "1 - 1" = "#E5C777") %>%
#   gather("group", "fill")
# 

bivariate_color_scale <- tibble("3 - 3" = "#3F2949",
                                "2 - 3" = "#435786",
                                "1 - 3" = "#4885C1",
                                "3 - 2" = "#77324C",
                                "2 - 2" = "#806A8A",
                                "1 - 2" = "#89A1C8",
                                "3 - 1" = "#AE3A4E",
                                "2 - 1" = "#BC7C8F",
                                "1 - 1" = "#CABED0") %>%
  gather("group", "fill")


"#4885C1"
"#AE3A4E"

tmp <- blend_colors(c("#4885C1",
                      "#AE3A4E"),
                    do_plot = F)


ggplot() +
  geom_sf(data = hexa, fill = tmp)



na_col <- "gray90"
na_col <- "transparent"

plot_df <- hexa %>% 
  select(state, lost_pct, stressor_pct)




plot_df <- plot_df %>% 
  mutate(lost_quantiles = cut(lost_pct,
                              breaks = quantiles_lost,
                              include.lowest = TRUE),
         stressor_quantiles = cut(stressor_pct,
                                  breaks = quantiles_stressor,
                                  include.lowest = TRUE),
         group = paste(as.numeric(lost_quantiles), "-",
                       as.numeric(stressor_quantiles))) %>% 
  left_join(bivariate_color_scale, by = "group")




plot_df <- plot_df %>% 
  mutate(fill = ifelse(is.na(fill),
                       na_col,
                       fill))

honey_pal <- c("#E3D7C1", "#C8B188", "#C4952E", "#BE7C22", "#93500C")
bgk_col <- "goldenrod"
bgk_col <- honey_pal[1]
bgk_col <- colorspace::lighten("goldenrod", .05)

bi_legend <- bivariate_color_scale %>% 
  separate(group, into = c("lost_pct", "stressor_pct"), sep = " - ") %>%
  mutate(lost_pct = as.integer(lost_pct),
         stressor = as.integer(stressor_pct))

legend <- ggplot() +
  geom_tile(data = bi_legend,
            mapping = aes(x = lost_pct,
                          y = stressor_pct,
                          fill = fill)) +
  scale_fill_identity() +
  labs(x = expression("More colonies lost" %->% ""),
       y = expression("Higher stressor" %->% "")) +
  theme_map() +
  theme(axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6,
                                    angle = 90)) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "transparent",
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       color = "transparent"))

map <- ggplot() +
  geom_sf(data = plot_df, aes(fill = fill), color = "white", size = .5) +
  #geom_sf_text(data = cents, aes(label = iso3166_2), color = "white") +
  geom_shadowtext(data = cents_raw, aes(x = X, y = Y, label = label)) +
  scale_fill_identity(na.value = "pink") +
  theme_void() +
  theme(panel.background = element_rect(fill = bgk_col,
                                        color = bgk_col),
        plot.background = element_rect(fill = bgk_col,
                                       color = bgk_col),
        plot.margin = margin(0, 0, 80, 0))


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.775, 0.025, 0.225, 0.225)

