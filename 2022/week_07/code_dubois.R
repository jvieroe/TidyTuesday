library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(scales)
library(colorspace)
library(ggtext)

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


df <- df %>% 
  mutate(x = factor("x")) %>% 
  mutate(xx = row_number())

insert_comma <- scales::label_comma(accuracy = 1, big.mark = ",", decimal.mark = ".")

df <- df %>%
  mutate(tmp = c("\u2013\u2013\u2013\u2013\u2013\u2013",
                 "\u2013\u2013\u2013\u2013",
                 "\u2013\u2013\u2013\u2013",
                 "\u2013\u2013",
                 "\u2013\u2013",
                 "\u2013\u2013")) %>%
  mutate(num = insert_comma(houshold_value_dollars)) %>% 
  mutate(num = as.character(num)) %>% 
  mutate(dollars = paste(" </span>$", num, sep = " ")) %>% 
  mutate(dollars = str_pad(dollars,
                           max(nchar(dollars)),
                           side ="left")) %>%
  mutate(label = paste(year, tmp, dollars,
                       sep = " ")) %>% 
  mutate(label = paste("<span style='font-family:Quantico'>",
                       year,
                       "</span>",
                       tmp,
                       "<span style='font-family:Quantico'>",
                       dollars,
                       "</span>",
                       sep = " "))


dubois_pal <- c("#ffc0cb",
                "#4682b4",
                "#654321",
                "#ffd700",
                "#d2b48c",
                "#dc143c")


show_col(dubois_pal)

background_col <- lighten("#d2b48c", 0.8)



font_regular <- "Chakra Petch"
font_base <- "Helvetica"
font_title = "Quantico"
font_col <- "#393433"





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
  geom_richtext(data = df,
                aes(x = rev(x),
                    y = 0,
                    group = desc(year),
                    label = label),
                position = position_dodge(0.35),
                hjust = 1,
                label.size = 0,
                size = 3,
                fill = "transparent",
                family = font_base) +
  expand_limits(y = max_y * 1.15) +
  scale_x_discrete(expand = c(0.075, 1)) +
  scale_fill_manual(values = dubois_pal) +
  labs(title = "ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE<br>OWNED BY GEORGIA AFRICAN AMERICANS",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 7 | Data: Anthony Starks") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill = background_col,
                                       color = background_col),
        panel.background = element_rect(fill = background_col,
                                        color = background_col),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = -175,
                                      r = -150,
                                      b = -550,
                                      l = -150,
                                      unit = "pt"),
        plot.caption = ggtext::element_markdown(size = 8,
                                                color = font_col,
                                                family = font_regular,
                                                hjust = 0.5,
                                                margin = ggplot2::margin(b = -175,
                                                                         t = -200,
                                                                         unit = "pt")),
        plot.title = ggtext::element_markdown(size = 20,
                                              color = font_col,
                                              family = font_regular,
                                              hjust = 0.5,
                                              margin = ggplot2::margin(t = 190,
                                                                       b = -285,
                                                                       unit = "pt")))



ggsave(plot = last_plot(),
       filename = "2022/week_07/dubois.png")

