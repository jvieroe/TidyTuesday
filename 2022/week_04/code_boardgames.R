library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(ggtext)
library(MetBrewer)
library(wesanderson)

tt <- tidytuesdayR::tt_load('2022-01-25')

details <- tt[[1]]
ratings <- tt[[2]]

df <- ratings %>% 
  tidylog::left_join(., details,
                     by = "id")


rm(ratings, details, tt)

df <- df %>% 
  rownames_to_column()

df <- df %>% 
  mutate(across(c(boardgameartist, boardgamedesigner, boardgamecategory),
                ~ str_replace(.x, "\\[|\\]", ""))) %>% 
  mutate(across(c(boardgameartist, boardgamedesigner, boardgamecategory),
                ~ str_remove_all(.x, "\'"))) %>% 
  mutate(across(c(boardgameartist, boardgamedesigner, boardgamecategory),
                ~ str_replace(.x, "\\[|\\]", ""))) %>% 
  mutate(across(c(boardgameartist, boardgamedesigner, boardgamecategory),
                ~ str_squish(.x)))

df_long <- df %>% 
  separate_rows(boardgamedesigner, sep = ",") %>% 
  mutate(boardgamedesigner = str_squish(boardgamedesigner)) %>% 
  arrange(yearpublished, boardgamedesigner)

df_long <- df_long %>% 
  filter(boardgamedesigner != "(Uncredited)") %>% 
  filter(!is.na(boardgamedesigner))


df_designer <- df_long %>% 
  group_by(boardgamedesigner) %>% 
  summarise(sum_owned = sum(owned, na.rm = TRUE),
            mean_rating = mean(average, na.rm = TRUE),
            mean_bayes = mean(bayes_average, na.rm = TRUE),
            games_produced = n(),
            mean_year = mean(yearpublished, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(games_produced),
          desc(sum_owned),
          desc(mean_rating))


plot_df <- df_designer %>% 
  slice_head(n = 100)


cat_df <- df_long %>% 
  filter(boardgamedesigner %in% plot_df$boardgamedesigner) %>% 
  select(boardgamedesigner, primary, boardgamecategory) %>% 
  separate_rows(boardgamecategory, sep = ",") %>% 
  mutate(boardgamecategory = str_squish(boardgamecategory)) %>% 
  arrange(boardgamedesigner, primary, boardgamecategory)


cat_df <- cat_df %>% 
  group_by(boardgamedesigner, boardgamecategory) %>% 
  summarise(n_cat = n()) %>% 
  ungroup() %>% 
  group_by(boardgamedesigner) %>% 
  slice(which.max(n_cat)) %>% 
  ungroup() %>% 
  arrange(desc(n_cat),
          boardgamedesigner)

plot_df_new <- plot_df %>% 
  tidylog::left_join(., cat_df,
                     by = "boardgamedesigner")


plot_df_new <- plot_df_new %>% 
  mutate(wargame = ifelse(boardgamecategory == "Wargame", TRUE, FALSE)) %>% 
  mutate(cat = case_when(boardgamecategory == "Card Game" ~ "Card Game",
                         boardgamecategory == "Wargame" ~ "Wargame",
                         TRUE ~ "Other")) %>% 
  mutate(cat = factor(cat,
                      levels = c("Card Game",
                                 "Wargame",
                                 "Other")))


knizia <- plot_df_new %>% 
  slice(which.max(games_produced))

konieczka <- plot_df_new %>% 
  slice(which.max(mean_bayes))

pal <- wes_palette("Zissou1")
pal <- pal[c(5, 3, 2)]

font <- "Ubuntu Mono"
font_title <- "Righteous"

text_col1 <- "white"
text_col2 <- "gray90"
bgk_col <- "gray15"
grid_col <- "gray90"

plot_df_new %>% 
  group_by(boardgamecategory) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

ggplot() +
  geom_point(data = plot_df_new,
             aes(x = log(games_produced), 
                 y = log(sum_owned),
                 size = mean_bayes,
                 color = cat),
             shape = 21,
             alpha = 1) + 
  geom_point(data = plot_df_new,
             aes(x = log(games_produced), 
                 y = log(sum_owned),
                 size = mean_bayes,
                 color = cat,
                 fill = cat),
             shape = 21,
             alpha = .55) + 
  scale_color_manual(values = pal,
                     name = "") +
  scale_fill_manual(values = pal,
                    name = "") +
  scale_size_continuous(range = c(1, 12)) +
  scale_x_continuous(breaks = seq(3.5, 6, 0.5),
                     labels = format(round(seq(3.5, 6, 0.5), 1), nsmall = 1)) +
  labs(x = "Games Produced (log)",
       y = "Ownership (log)",
       title = "<span style='color:#45f248'>Reiner Knizia</span>: the GOAT of board game design",
       subtitle = "The 100 most productive board game designers of all time and the board game category<br>
       most frequently associated with their games. Across the board (pun intended) the most<br>frequent categories are
       <span style='color:#F21A00'>'Card Game'</span> and <span style='color:#EBCC2A'>'Wargame'</span>",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 4 | Data: Board Games Geek (via Kaggle)") +
  geom_point(data = knizia,
             aes(x = log(games_produced), 
                 y = log(sum_owned)),
             fill = NA,
             shape = 21,
             size = 14, color = text_col1) +
  geom_segment(data = knizia, aes(x = (log(games_produced)-0.06), xend = (log(games_produced)-0.25),
                                 y = log(sum_owned), yend = log(sum_owned)),
               color = text_col1) +
  geom_segment(data = knizia, aes(x = (log(games_produced)-0.25), xend = (log(games_produced)-0.25),
                                 y = log(sum_owned), yend = (log(sum_owned))-0.75),
               color = text_col1) +
  geom_richtext(data = knizia, aes(x = (log(games_produced)-0.75),
                                   y = (log(sum_owned))-1.25),
                label = "Reiner Knizia is the most productive board<br>game designer with <span style='color:#45f248'>329 games</span>
                in the<br><b><i>Board Games Geek</i></b> database. He also<br>ranks #1 on ownership with a total of<br>almost <span style='color:#45f248'>700,000</span>",
                size = 4,
                hjust = 0,
                label.color = NA,
                text.color = text_col1,
                fill = NA, alpha = 1) +
  geom_point(data = konieczka,
             aes(x = log(games_produced), 
                 y = log(sum_owned)),
             fill = NA,
             shape = 21,
             size = 20, color = text_col1) +
  geom_segment(data = konieczka, aes(x = log(games_produced), xend = log(games_produced),
                                     y = (log(sum_owned) + 0.26), yend = log(sum_owned)+0.75),
               color = text_col1) +
  geom_segment(data = konieczka, aes(x = log(games_produced), xend = (log(games_produced) + 0.25),
                                     y = (log(sum_owned) + 0.75), yend = log(sum_owned) + 0.75),
               color = text_col1) +
  geom_richtext(data = konieczka, aes(x = (log(games_produced) + 0.275),
                                      y = (log(sum_owned)) + 0.75),
                label = "Corey Konieczka has the highest Bayes Average rating (<span style='color:#45f248'>6.81</span>) across<br>his 27 games in the <b><i>Board Games Geek</i></b> database",
                size = 4,
                hjust = 0,
                label.color = NA,
                text.color = text_col1,
                fill = NA, alpha = 1) +
  theme_minimal() +
  guides(size = "none",
         color = guide_legend(override.aes = list(size = 6))) +
  theme(plot.title = ggtext::element_markdown(color = text_col1,
                                              size = 26,
                                              family = font_title),
        plot.subtitle = ggtext::element_markdown(color = text_col1,
                                                 size = 14,
                                                 family = font,
                                                 margin = ggplot2::margin(t = 5, r = 0, b = 15, l = 0)),
        plot.caption = ggtext::element_markdown(color = text_col2,
                                                family = font),
        panel.background = element_rect(fill = bgk_col, color = bgk_col),
        plot.background = element_rect(fill = bgk_col, color = bgk_col),
        panel.grid.minor = element_line(color = grid_col,
                                        size = .025),
        panel.grid.major = element_line(color = grid_col,
                                        size = .1),
        axis.title.y = element_text(color = text_col2,
                                    family = font,
                                    size = 12,
                                    margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(color = text_col2,
                                    family = font,
                                    size = 12,
                                    margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(color = text_col2,
                                   family = font,
                                   size = 10),
        axis.text.x = element_text(color = text_col2,
                                   family = font,
                                   size = 10),
        legend.position = "bottom",
        legend.text = element_text(color = text_col2,
                                   family = font))

ggsave(plot = last_plot(),
       filename = "2022/week_04/boardgames.png",
       dpi = 400)

