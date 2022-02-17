
library(tidyverse)
library(tidytuesdayR)
library(janitor)
#library(spiralize)
library(scales)
library(colorspace)

# https://flowingdata.com/2022/01/10/a-quick-and-easy-way-to-make-spiral-charts-in-r/
# https://stackoverflow.com/questions/52939337/how-to-create-a-time-series-spiral-graph-using-r
# https://www.michaelxiu.com/2020/08/17/ggplot2-Basics/
# https://bydata.github.io/nyt-corona-spiral-chart/

# https://stackoverflow.com/questions/35225461/zoom-scale-with-coord-polar
# https://jokergoo.github.io/spiralize_vignettes/spiralize.html

df <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge08/data.csv")

df <- df %>% 
  clean_names()

df

df <- df %>%
  mutate(year = factor(year))

levels(df$year)

ggplot(df) +
  geom_bar(aes(x = year,
               y = houshold_value_dollars),
           stat = "identity")


ggplot(df) +
  geom_bar(aes(x = year,
               y = houshold_value_dollars,
               fill = year),
           stat = "identity")


ggplot(df) +
  geom_bar(aes(x = fct_reorder(year, desc(houshold_value_dollars)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity")


ggplot(df) +
  geom_bar(aes(x = fct_reorder(year, desc(houshold_value_dollars)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity") +
  coord_polar()


ggplot(df) +
  geom_bar(aes(x = fct_reorder(year, desc(houshold_value_dollars)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity") +
  coord_polar(theta = "y")

max_y <- df %>% 
  slice(which.max(houshold_value_dollars)) %>% 
  pull(houshold_value_dollars)

max_x <- df %>% 
  slice(which.max(year)) %>% 
  pull(year) %>% 
  as.numeric()


ggplot(df) +
  geom_bar(aes(x = fct_reorder(year, desc(houshold_value_dollars)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity") +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.5)

exp <- tibble(year = c(seq(1875, 1925, 5),
                       1899)) %>% 
  arrange(year) %>% 
  mutate(year = factor(year)) %>% 
  tidylog::left_join(.,
                     df,
                     by = "year")

ggplot(exp) +
  geom_bar(aes(x = fct_reorder(year, desc(year)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity",
           width = .5) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.2)


ggplot(exp) +
  geom_bar(aes(x = fct_reorder(year, desc(year)),
               y = houshold_value_dollars,
               fill = year),
           stat = "identity",
           width = .5) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1)


ggplot(exp) +
  geom_bar(aes(x = fct_reorder(year, desc(year)),
               y = houshold_value_dollars,
               fill = year,
               group = year),
           stat = "identity",
           width = 0.5,
           position = position_dodge(0.9)) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15)

exp <- exp %>% 
  mutate(x = "x")

ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year,
               group = year),
           #stat = "identity",
           width = 0.5,
           position = position_dodge(0.9))

ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year),
           #stat = "identity",
           width = 0.5,
           position = position_dodge(0.5))

ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           width = 0.5,
           position = position_dodge(0.5)) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15)

getwd()
ggsave(plot = last_plot(),
       filename = "test_1.png")


dubois_pal <- c("#ffc0cb",
                "#4682b4",
                "#654321",
                "#ffd700",
                "#d2b48c",
                "#dc143c")


dubois_pal <- c(dubois_pal,
                rep("#000000",
                    (length(exp$year) - length(dubois_pal))))

show_col(dubois_pal)

background_col <- lighten("#d2b48c", 0.8)

ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           width = 0.5,
           color = "#000000",
           size = 0.1,
           position = position_dodge(0.5),
           alpha = 0.8) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15) +
  scale_fill_manual(values = dubois_pal) +
  theme_void() + 
  theme(plot.background = element_rect(fill = background_col,
                                       color = background_col),
        legend.position = "none")


ggsave(plot = last_plot(),
       filename = "test_2.png")


ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           #width = 0.1,
           color = "#000000",
           size = 0.1,
           position = position_dodge(width = 1),
           alpha = 0.8) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15) +
  scale_fill_manual(values = dubois_pal) +
  theme_void() + 
  theme(plot.background = element_rect(fill = background_col,
                                       color = background_col),
        legend.position = "none")

ggplot(exp) +
  geom_col(aes(x = x,
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           width = 0.2,
           color = "#000000",
           size = 0.1,
           position = position_dodge2(width = 0.2,
                                      preserve = "single"),
           alpha = 0.8) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15) +
  scale_fill_manual(values = dubois_pal) +
  theme_void() + 
  theme(plot.background = element_rect(fill = background_col,
                                       color = background_col),
        legend.position = "none")


ggsave(plot = last_plot(),
       filename = "test_3.png")



# ---------------------------------------

df <- df %>% 
  mutate(x = factor("x")) %>% 
  mutate(xx = row_number())

ggplot(df) +
  geom_col(aes(x = rev(xx),
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           width = 0.5,
           color = "#000000",
           size = 0.1,
           position = position_dodge(0.5),
           alpha = 0.8) +
  coord_polar(theta = "y") +
  expand_limits(y = max_y * 1.15) +
  scale_x_continuous(limits = c(-2, 7)) +
  scale_fill_manual(values = dubois_pal) +
  theme_void() + 
  theme(plot.background = element_rect(fill = background_col,
                                       color = background_col),
        legend.position = "none")

ggsave(plot = last_plot(),
       filename = "test_4.png")


ggplot(df) +
  geom_col(aes(x = rev(x),
               y = houshold_value_dollars,
               fill = year,
               group = desc(year)),
           width = 0.35,
           color = "#000000",
           size = 0.1,
           position = position_dodge(0.35),
           alpha = 0.8) +
  coord_polar(theta = "y",
              clip = "on") +
  expand_limits(y = max_y * 1.15) +
  scale_x_discrete(expand = c(0.075, 1)) +
  scale_fill_manual(values = dubois_pal) +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = background_col,
                                       color = background_col),
        panel.background = element_rect(fill = background_col,
                                        color = background_col),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = -150,
                                      r = -150,
                                      b = -150,
                                      l = -150,
                                      unit = "pt"))


ggsave(plot = last_plot(),
       filename = "test_5.png")

# https://stackoverflow.com/questions/35225461/zoom-scale-with-coord-polar
# https://jokergoo.github.io/spiralize_vignettes/spiralize.html