library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(ggtext)
library(MetBrewer)
library(wesanderson)

# tt <- tidytuesdayR::tt_load('2022-01-25')
# 
# details <- tt[[1]]
# ratings <- tt[[2]]
# 
# rm(tt)
# 
# saveRDS(details, "2022/week_04/data/details.rds")
# saveRDS(ratings, "2022/week_04/data/ratings.rds")


details <- readRDS("2022/week_04/data/details.rds")
ratings <- readRDS("2022/week_04/data/ratings.rds")



df <- ratings %>% 
  tidylog::left_join(., details,
                     by = "id")


rm(ratings, details)

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



ggplot(plot_df, aes(x = games_produced, y = sum_owned)) +
  geom_point(aes(size = mean_bayes), alpha = .5)

p1 <- last_plot()

ggplot(plot_df, aes(x = log(games_produced), y = log(sum_owned))) +
  geom_point(aes(size = mean_bayes,
                 color = mean_year), 
             alpha = .5)

p2 <- last_plot()



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

plot_df_new %>% 
  slice(which.max(sum_owned))


plot_df %>% filter(boardgamedesigner == "Reiner Knizia")

pal <- MetBrewer::met.brewer("Homer2")
pal <- wes_palette("Zissou1")
pal <- pal[c(2, 3, 5)]

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
  labs(x = "Games Produced (log)",
       y = "Ownership (log)") +
  geom_point(data = knizia,
             aes(x = log(games_produced), 
                 y = log(sum_owned)),
             fill = NA,
             shape = 21,
             size = 14, color = "white") +
  geom_segment(data = knizia, aes(x = (log(games_produced)-0.07), xend = (log(games_produced)-0.25),
                                 y = log(sum_owned), yend = log(sum_owned)),
               color = "white") +
  geom_segment(data = knizia, aes(x = (log(games_produced)-0.25), xend = (log(games_produced)-0.25),
                                 y = log(sum_owned), yend = (log(sum_owned))-0.75),
               color = "white") +
  geom_richtext(data = knizia, aes(x = (log(games_produced)-0.80),
                                   y = (log(sum_owned))-1.1),
                label = "Reiner Knizia is the most productive board<br>game designer with more than 300 games<br>
                in the Board Games Geek database. He<br>also ranks #1 for ownership",
                size = 3,
                hjust = 0,
                label.color = NA,
                text.color = "white",
                fill = NA, alpha = 1) +
  theme_minimal() +
  guides(size = "none",
         color = guide_legend(override.aes = list(size = 6))) +
  theme(plot.title = ggtext::element_markdown(color = "white",
                                              size = 24),
        plot.subtitle = ggtext::element_markdown(color = "white",),
        plot.caption = ggtext::element_markdown(color = "gray90",),
        panel.background = element_rect(fill = "gray15", color = "gray15"),
        plot.background = element_rect(fill = "gray15", color = "gray15"),
        panel.grid.minor = element_line(color = "gray70",
                                        size = .025),
        panel.grid.major = element_line(color = "gray70",
                                        size = .1),
        axis.title.y = element_text(color = "gray90",
                                    size = 12,
                                    margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(color = "gray90",
                                    size = 12,
                                    margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(color = "gray90",
                                   size = 10),
        axis.text.x = element_text(color = "gray90",
                                   size = 10),
        legend.position = "bottom",
        legend.text = element_text(color = "gray90"))



ggsave(plot = last_plot(),
       filename = "2022/week_04/boardgames.png",
       dpi = 400)

run <- plot_df_new %>% 
  group_by(boardgamecategory) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
