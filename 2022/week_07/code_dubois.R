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


max_y <- df %>% 
  slice(which.max(houshold_value_dollars)) %>% 
  pull(houshold_value_dollars)

max_x <- df %>% 
  slice(which.max(year)) %>% 
  pull(year) %>% 
  as.numeric()


dubois_pal <- c("#ffc0cb",
                "#4682b4",
                "#654321",
                "#ffd700",
                "#d2b48c",
                "#dc143c")


show_col(dubois_pal)

background_col <- lighten("#d2b48c", 0.8)


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



df <- df %>% 
  mutate(tmp = c("\u2013\u2013\u2013\u2013\u2013",
                 "\u2013\u2013\u2013\u2013",
                 "\u2013\u2013\u2013\u2013",
                 "\u2013\u2013\u2013",
                 "\u2013\u2013\u2013",
                 "\u2013\u2013\u2013")) %>% 
  mutate(num = as.character(houshold_value_dollars)) %>% 
  mutate(num = format(num,
                      nsmall = 1,
                      big.mark = ",")) %>% 
  mutate(label = paste(year, tmp, "$", houshold_value_dollars,
                       sep = " "))

ggplot() +
  geom_col(data = df, aes(x = rev(x),
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
  #annotate()
  geom_label(data = df,
             aes(x = rev(x),
                 y = 0,
                 group = desc(year),
                 label = label),
             position = position_dodge(0.35),
             hjust = 1,
             label.size = 0,
             size = 3,
             fill = "transparent") +
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
       filename = "2022/week_07/dubois.png")

