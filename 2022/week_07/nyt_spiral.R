library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

# rm(list = ls())


owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
country <- "United States"
covid <- read_csv(owid_url)

covid_cases <- covid %>% 
  filter(location == country) %>% 
  select(date, new_cases, new_cases_smoothed) %>% 
  arrange(date) %>% 
  # Add the dates before the 1st confirmed case
  add_row(date = as_date("2020-01-01"), new_cases = 0, new_cases_smoothed = 0,
          .before = 1) %>% 
  complete(date = seq(min(.$date), max(.$date), by = 1),
           fill = list(new_cases = 0, new_cases_smoothed = 0)) %>% 
  mutate(day_of_year = yday(date),
         year = year(date)
  )



p <- covid_cases %>% 
  ggplot() +
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date))) +
  coord_polar()
p






n <- 1000
i <- 2

df <- tibble(x = rep(seq(1, n, 1),
                     i),
             y = rnorm(mean = 5, sd = 0.5,
                       n = (n * i)))

df

df %>% 
  ggplot(.) +
  geom_segment(aes(x = x, xend = x + 1,
                   y = y, yend = y))


