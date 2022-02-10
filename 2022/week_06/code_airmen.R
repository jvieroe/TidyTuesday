library(tidyverse)
library(janitor)
library(lubridate)
library(ggstream)
library(scales)



airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

class(airmen$graduation_date)

airmen$graduation_date[1]

airmen <- airmen %>% 
  mutate(date = ymd(graduation_date)) %>% 
  arrange(date)

airmen <- airmen %>% 
  mutate(pilot_type = ifelse(pilot_type == "Liason pilot",
                             "Liaison pilot",
                             pilot_type))



airmen_sum <- airmen %>% 
  filter(!is.na(date)) %>% 
  mutate(t = 1) %>% 
  group_by(date, pilot_type) %>% 
  summarize(sum = n()) %>% 
  mutate(sum_count = cumsum(sum)) %>% 
  ungroup()


airmen_sum_2 <- airmen %>% 
  filter(!is.na(date)) %>% 
  mutate(t = 1) %>% 
  group_by(date) %>% 
  summarize(sum = n()) %>% 
  mutate(sum_count = cumsum(sum)) %>% 
  ungroup()


ggplot(airmen_sum) +
  geom_line(aes(x = date, y = sum_count, color = pilot_type))


ggplot(airmen_sum_2) +
  geom_line(aes(x = date, y = sum_count))


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





