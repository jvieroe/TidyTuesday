library(tidyverse)
library(janitor)
library(lubridate)
library(ggstream)
library(scales)

rm(list = ls())

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

test <- airmen %>% 
  filter(!is.na(tally_num)) %>% 
  mutate(sum = sum(tally_num))

tabyl(test$class)

airmen_sum <- airmen %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarize(tally_num = sum(tally_num, na.rm = TRUE)) %>%
  ungroup()



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
  rowwise() %>% 
  mutate(tally = sum(c(tmp, tally_num),
                     na.rm = TRUE)) %>% 
  ungroup()

plot_df <- plot_df %>% 
  mutate(tally_cs = cumsum(tally))

ggplot(plot_df) +
  geom_line(aes(x = date, y = tally_cs))


ggplot(plot_df) +
  geom_area(aes(x = date, y = tally_cs))




dubois <- c("#e23653",
            "#fcb800",
            "#577565")

dubois_full <- c("#dd374f",
                 "#efb64c",
                 "#dbcbb7",
                 "#9b9a94",
                 "#a3a5b6",
                 "#ba9a82",
                 "#39518a",
                 "#f1ac01")

ggplot(airmen_sum) +
  geom_stream(aes(x = date, y = sum_count, fill = pilot_type),
              type = "mirror",
              alpha = 1,
              color = "black",
              size = .25) +
  theme_minimal() +
  scale_fill_manual(values = dubois,
                    name = "") +
  theme(plot.background = element_rect(fill = dubois_full[3]))


colors1 <- c("#dd374f",
             "#efb64c",
             "#dbcbb7",
             "#9b9a94",
             "#a3a5b6",
             "#ba9a82",
             "#39518a",
             "#f1ac01")

colors2 <- c("#e23653",
             "#fcb800",
             "#577565")

show_col(colors1[1])
show_col(colors1[2])
show_col(colors1[3])
show_col(colors1[4])
show_col(colors1[5])
show_col(colors1[6])
show_col(colors1[7])
show_col(colors1[8])

show_col(colors2[1])
show_col(colors2[2])
show_col(colors2[3])





