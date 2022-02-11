library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

airmen <- airmen %>%
  mutate(graduation_date = ymd(graduation_date)) %>%
  arrange(graduation_date)


airmen <- airmen %>% 
  separate_rows(aerial_victory_credits,
                sep = ";")

airmen <- airmen %>% 
  mutate(date = sub(".*on ", "", aerial_victory_credits)) %>% 
  mutate(date = mdy(date))

airmen <- airmen %>% 
  separate_rows(aerial_victory_credits,
                sep = "and")

airmen <- airmen %>% 
  mutate(aerial_victory_credits = str_squish(aerial_victory_credits))


airmen <- airmen %>% 
  mutate(tally = sub(".*Downed ", "", aerial_victory_credits)) %>% 
  mutate(tally = str_sub(tally, 1, 5)) %>% 
  mutate(tally = str_replace(tally,
                             pattern = " 1/2",
                             replacement = ".5")) %>% 
  mutate(tally = str_replace(tally,
                             pattern = "1/2",
                             replacement = "0.5")) %>% 
  mutate(tally_num = parse_number(tally))

airmen <- airmen %>% 
  arrange(graduation_date)

airmen <- airmen %>% 
  mutate(class_group = str_sub(class, 1, 5))


test <- airmen %>% 
  filter(!is.na(tally_num)) %>% 
  mutate(sum = sum(tally_num))

tabyl(test$class_group)

airmen_sum <- airmen %>% 
  filter(!is.na(date)) %>% 
  group_by(date, class_group) %>% 
  summarize(tally_num = sum(tally_num, na.rm = TRUE)) %>% 
  ungroup()

airmen_sum <- airmen_sum %>%
  pivot_wider(id_cols = date,
              names_from = class_group,
              values_from = tally_num) %>% 
  clean_names()

date_range_min <- airmen %>% 
  arrange(graduation_date) %>% 
  slice(which.min(graduation_date)) %>% 
  select(date = graduation_date)

date_range_max_1 <- airmen %>% 
  arrange(graduation_date) %>% 
  slice(which.max(graduation_date)) %>% 
  select(date = graduation_date)

date_range_max_2 <- airmen_sum %>% 
  arrange(date) %>% 
  slice(which.max(date)) %>% 
  select(date)

date_range_max <- rbind(date_range_max_1,
                        date_range_max_2) %>% 
  slice(which.max(date))

date_range <- rbind(date_range_min,
                    date_range_max)

date_range <- date_range %>% 
  complete(., date = full_seq(date, 1)) %>% 
  mutate(tmp = 0)

plot_df <- 
  tidylog::left_join(date_range,
                     airmen_sum,
                     by = "date")

plot_df <- plot_df %>% 
  pivot_longer(cols = starts_with("se_"),
               names_to = "class_group",
               values_to = "tally_num")

plot_df <- plot_df %>% 
  rowwise() %>% 
  mutate(tally = sum(c(tmp, tally_num),
                     na.rm = TRUE)) %>% 
  ungroup()

plot_df <- plot_df %>% 
  arrange(class_group, date) %>% 
  group_by(class_group) %>% 
  mutate(tally_cs = cumsum(tally)) %>% 
  ungroup()

plot_df <- plot_df %>% 
  filter(!is.na(class_group))

plot_df <- plot_df %>% 
  arrange(class_group, tally_cs)


plot_df <- plot_df %>% 
  mutate(class_group = case_when(class_group == "se_42" ~ "SE-42",
                                 class_group == "se_43" ~ "SE-43",
                                 class_group == "se_44" ~ "SE-44"))



ggplot(plot_df) +
  geom_line(aes(x = date, y = tally_cs, color = class_group),
            size = 1) +
  scale_fill_manual(values = dubois,
                    name = "Aircraft class series")


# line segments
min_x <- plot_df %>% 
  slice(which.min(date)) %>% 
  pull(date)

max_x <- plot_df %>% 
  slice(which.max(date)) %>% 
  pull(date)

min_y <- plot_df %>% 
  slice(which.min(tally_cs)) %>% 
  pull(tally_cs)

max_y <- plot_df %>% 
  slice(which.max(tally_cs)) %>% 
  pull(tally_cs)


# colors
dubois <- c("#e23653",
            "#fcb800",
            "#577565",
            "#dd374f",
            "#efb64c",
            "#dbcbb7",
            "#9b9a94",
            "#a3a5b6",
            "#ba9a82",
            "#39518a",
            "#f1ac01")

show_col(dubois)

background <- dubois[6]
foreground <- dubois[1]
grid_stroke = 0.075


# fonts
font_regular <- "Chakra Petch"
font_title = "Orbitron"
font_col <- "#393433"


# ----- plot 
ggplot(plot_df) +
  geom_area(aes(x = date, y = tally_cs, fill = class_group),
            position = 'stack',
            color = "black",
            size = .4) +
  geom_segment(aes(x = max_x, xend = max_x,
                   y = min_y, yend = max_y),
               size = 0.4) +
  geom_segment(aes(x = min_x, xend = max_x,
                   y = min_y, yend = min_y),
               size = 0.2) +
  scale_fill_manual(values = dubois,
                    name = "AIRCRAFT CLASS SERIES") +
  scale_y_continuous(breaks = seq(0, 120, 20),
                     labels = seq(0, 120, 20),
                     limits = c(0, 120)) +
  scale_x_date(date_minor_breaks = "1 year") +
  labs(title = "THE TUSKEGEE AIRMEN",
       y = "CUMULATIVE SUM OF AIRCRAFTS DOWNED",
       caption = "GRAPHICS: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 6 | DATA: Commemorative Airforce (CAF) by way of the VA-TUG") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = background,
                                       color = background),
        panel.background = element_rect(fill = background,
                                        color = background),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(family = font_regular,
                                   color = font_col,
                                   size = 9),
        legend.text = element_text(family = font_regular,
                                   color = font_col),
        legend.title = element_text(family = font_regular,
                                    color = font_col),
        axis.text.y = element_text(family = font_regular,
                                   color = font_col,
                                   size = 9),
        axis.title.y = element_text(family = font_regular,
                                    color = font_col,
                                    vjust = 5,
                                    hjust = 0.5),
        axis.title.x = element_blank(),
        plot.title = element_text(family = font_title,
                                  size = 35,
                                  color = font_col),
        plot.caption = ggtext::element_markdown(size = 10,
                                                color = font_col,
                                                family = font_regular,
                                                hjust = 0,
                                                margin = ggplot2::margin(t = 30, 
                                                                         unit = "pt")),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(l = 20, b = 20, t = 10, 
                                      unit = "pt"))


ggsave(plot = last_plot(),
       "2022/week_06/tuskegee_airmen.png",
       dpi = 400,
       width = 12,
       height = 9)




# SIMPLE PLOT
plot_df_comb <- plot_df %>% 
  group_by(date) %>% 
  summarize(tally_cs = sum(tally_cs)) %>% 
  ungroup()


ggplot(plot_df) +
  geom_area(data = plot_df_comb, aes(x = date, y = tally_cs),
            fill = NA,
            color = "black",
            size = .4) +
  geom_segment(aes(x = max_x, xend = max_x,
                   y = min_y, yend = max_y),
               size = 0.4) +
  geom_segment(aes(x = min_x, xend = max_x,
                   y = min_y, yend = min_y),
               size = 0.4) +
  scale_fill_manual(values = dubois,
                    name = "AIRCRAFT CLASS SERIES") +
  scale_y_continuous(breaks = seq(0, 120, 20),
                     labels = seq(0, 120, 20),
                     limits = c(0, 120)) +
  scale_x_date(date_minor_breaks = "1 year") +
  labs(title = "TUSKEGEE AIRMEN",
       y = "CUMULATIVE SUM OF AIRCRAFTS DOWNED",
       caption = "GRAPHICS: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 6 | DATA: Commemorative Airforce (CAF) by way of the VA-TUG") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = background,
                                       color = background),
        panel.background = element_rect(fill = background,
                                        color = background),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(family = font_regular,
                                   color = font_col,
                                   size = 9),
        legend.text = element_text(family = font_regular,
                                   color = font_col),
        legend.title = element_text(family = font_regular,
                                    color = font_col),
        axis.text.y = element_text(family = font_regular,
                                   color = font_col,
                                   size = 9),
        axis.title.y = element_text(family = font_regular,
                                    color = font_col,
                                    vjust = 5,
                                    hjust = 0.5),
        axis.title.x = element_blank(),
        plot.title = element_text(family = font_title,
                                  size = 35,
                                  color = font_col),
        plot.caption = ggtext::element_markdown(size = 10,
                                                color = font_col,
                                                family = font_regular,
                                                hjust = 0,
                                                margin = ggplot2::margin(t = 30, 
                                                                         unit = "pt")),
        legend.position = "bottom",
        plot.margin = ggplot2::margin(l = 20, b = 20, t = 10, 
                                      unit = "pt"))






