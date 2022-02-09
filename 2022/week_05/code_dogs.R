library(tidyverse)
library(janitor)
library(cowplot)
library(ggtext)
library(wesanderson)
library(MetBrewer)
library(colorspace)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- breed_traits %>% 
  clean_names() %>% 
  mutate(breed = str_squish(breed)) %>% 
  mutate(breed = str_replace(breed, pattern = "'", " ")) #%>% .[, c(1:5, 7, 10:16)]

breed_rank_all <- breed_rank_all %>% 
  clean_names() %>% 
  mutate(breed = str_squish(breed))

df <- breed_traits %>% 
  tidylog::left_join(.,
                     breed_rank_all,
                     by = "breed")

df <- df %>% 
  mutate(coat_length = str_squish(coat_length))

df <- df %>% 
  filter(!coat_length %in% c("Plott Hounds"))

df <- df %>% 
  rowwise() %>% 
  mutate(rank_mean = mean(c_across(ends_with("_rank")),
                          na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-ends_with("_rank"))

df <- df %>% 
  filter(grepl("schnauzer", breed, ignore.case = TRUE))

df <- df %>% 
  select(breed,
         coat_length,
         affectionate_with_family,
         good_with_young_children,
         good_with_other_dogs,
         shedding_level,
         coat_grooming_frequency,
         drooling_level,
         openness_to_strangers,
         playfulness_level,
         watchdog_protective_nature,
         adaptability_level,
         trainability_level,
         energy_level,
         barking_level,
         mental_stimulation_needs)


df <- df %>% 
  pivot_longer(-c(breed,
                  coat_length))



df <- df %>% 
  filter(name %in% c("coat_grooming_frequency",
                     "shedding_level",
                     "trainability_level",
                     "playfullness_level",
                     "energy_level",
                     "affectionate_with_family",
                     "good_with_young_children",
                     "good_with_other_dogs"))


df <- df %>% 
  mutate(name = case_when(name == "coat_grooming_frequency" ~ "Grooming frequency",
                          name == "shedding_level" ~ "Shedding",
                          name == "trainability_level" ~ "Trainability",
                          name == "playfullness_level" ~ "Playfullness",
                          name == "energy_level" ~ "Energy",
                          name == "affectionate_with_family" ~ "Affectionate w/ family",
                          name == "good_with_young_children" ~ "Good w/ young children",
                          name == "good_with_other_dogs" ~ "Good with dogs"))


df <- df %>% 
  arrange(breed, name) %>% 
  group_by(breed) %>% 
  mutate(seq_ = row_number()) %>% 
  ungroup()


n_cats <- length(unique(df$name))

pal <- met.brewer("Renoir", type = "discrete")
txt_col <- pal[1]
pal <- pal[c(2:8)]

bkg_col <- "#fdfad4"
bkg_col <- lighten(bkg_col, 0.8)

title_font <- "Permanent Marker"
txt_font <- "Inconsolata"


ggplot(df) +
  geom_segment(data = tibble(y = c(1, 3, 5)),
               aes(x = 0, xend = (n_cats + 0.5),
                   y = y, yend = y),
               linetype = "97",
               color = "grey70",
               size = .25) +
  geom_col(aes(x = seq_, y = value,
               fill = str_wrap(name,
                               15)),
           alpha = .75) +
  coord_polar() +
  theme_void() +
  scale_fill_manual(values = pal,
                    name = "") +
  scale_y_continuous(limits = c(-0.5, 5),
                     breaks = seq(1, 5, 1),
                     labels = seq(1, 5, 1)) +
  facet_wrap(~ breed, ncol = 2,
             strip.position = "top") +
  labs(title = "Schnauzers",
       subtitle = stringr::str_wrap(
         "The three types (or sizes) of Schnauzers and their characteristics and qualities as rated by the American Kennel Club on a 1-5 scale",
         50),
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 5 | Data: American Kennel Club") +
  theme(plot.title = ggtext::element_markdown(size = 76,
                                              color = txt_col,
                                              family = title_font,
                                              margin = margin(t = 10, unit = "pt")),
        plot.subtitle = element_text(size = 18,
                                     color = txt_col,
                                     family = txt_font,
                                     lineheight = .7,
                                     margin = margin(t = 7, b = 22, unit = "pt")),
        plot.caption = ggtext::element_markdown(size = 10,
                                                color = txt_col,
                                                family = txt_font,
                                                hjust = 4),
        legend.position = c(.825, .285),
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(0.5, "cm"),
        legend.text = element_text(size = 12,
                                   color = txt_col,
                                   family = txt_font,
                                   lineheight = .7,
                                   margin = margin(r = 10, unit = "pt")),
        panel.spacing.x = unit(2, "cm"),
        strip.text = element_text(size = 18,
                                  color = txt_col,
                                  family = txt_font),
        panel.background = element_rect(fill = bkg_col,
                                        color = bkg_col),
        plot.background = element_rect(fill = bkg_col,
                                       color = bkg_col),
        plot.margin = grid::unit(c(t = 0, r = -0, b = 2.5, l = -0), "mm")) +
  guides(fill = guide_legend(byrow = TRUE,
                             ncol = 2))


ggsave(plot = last_plot(),
       "2022/week_05/dogs.png",
       dpi = 400,
       width = 9,
       height = 9)
