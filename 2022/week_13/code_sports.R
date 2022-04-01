library(tidyverse)
library(janitor)
library(MetBrewer)
library(ggtext)


sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

sports <- sports %>% 
  arrange(institution_name,
          unitid,
          year)


exp_gap <- sports %>% 
  group_by(sports) %>% 
  summarize(exp_men = sum(exp_men, na.rm = TRUE),
            exp_women = sum(exp_women, na.rm = TRUE),
            total_exp_menwomen = sum(total_exp_menwomen, na.rm = TRUE)) %>% 
  ungroup()

exp_gap <- exp_gap %>% 
  filter(total_exp_menwomen > 0)

exp_gap <- exp_gap %>% 
  mutate(share_men = exp_men / total_exp_menwomen) %>% 
  mutate(share_women = exp_women / total_exp_menwomen)


exp_gap <- exp_gap %>% 
  pivot_longer(!c(sports, !starts_with("share")),
               values_to = "share",
               names_to = "binary_gender",
               names_prefix = "share_") %>% 
  arrange(sports)

exp_gap %>% 
  group_by(sports) %>% 
  summarize(t = sum(share)) %>%  
  tabyl(t)

exp_gap <- exp_gap %>% 
  mutate(share = ifelse(binary_gender == "men",
                        - share,
                        share))



exp_gap <- exp_gap %>% 
  arrange(sports,
          binary_gender) %>% 
  group_by(sports) %>% 
  mutate(men_women = dplyr::lag(share)) %>% 
  ungroup()


exp_gap <- exp_gap %>% 
  mutate(share = ifelse(is.na(share),
                        0,
                        share))


bkg_col <- "gray20"
bkg <- element_rect(fill = bkg_col,
                    color = bkg_col)
fg_col <- "gray90"


MetBrewer::display_all()

# pal <- met.brewer("Benedictus",
#                   type = "discrete")
# pal
# pal[4]
# pal[11]


pal <- met.brewer("Hiroshige",
                  type = "discrete")
pal

pal[4]
pal[7]



base_font <- "Ubuntu Mono"
title_font <- "Titillium Web"

txt_data <- exp_gap %>% 
  filter(binary_gender == "men")

ggplot() +
  geom_hline(yintercept = c(-1, -0.5, 0, 0.5, 1),
             linetype = "dashed",
             color = fg_col, alpha = .5) +
  geom_segment(data = exp_gap,
               aes(x = fct_reorder(sports,
                                   share),
                   xend = fct_reorder(sports,
                                      share),
                   y = share,
                   yend = 0,
                   color = binary_gender)) +
  geom_point(data = exp_gap,
             aes(x = fct_reorder(sports,
                                 share),
                 y = share,
                 fill = binary_gender,
                 color = binary_gender,
                 size = total_exp_menwomen),
             shape = 21) +
  scale_color_manual(values = c(pal[4], pal[7])) +
  scale_fill_manual(values = c(pal[4], pal[7])) +
  scale_size_continuous(range = c(.5, 8)) +
  geom_text(data = txt_data,
            aes(x = fct_reorder(sports,
                                share),
                y = share,
                label = sports),
            color = fg_col,
            family = base_font,
            hjust = 1,
            size = 4,
            nudge_y = -.035,
            nudge_x = .1) +
  coord_flip() +
  labs(x = "",
       y = "Share",
       title = "Equity in College Athletics Expenditure?",
       subtitle = "Share of total expenditures allocated to <span style='color:#ffd06f;font-size:18px'>Men's</span> and <span style='color:#72bcd5;font-size:18px'>Women's</span> collegiate sports budgets (2015-2019) by sport.<br>
       Size represents total expenditure for each sport. The majority of college sports have expenditures that are skewed towards<br><span style='color:#72bcd5'>women's athletics</span> &#8212; but disciplines with the largest 
       total expenditures see money predominantly being allocated to <span style='color:#ffd06f;font-size:16px'>men's athletics</span>.",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | #TidyTuesday 2022, Week 13 | Data: Equity in Athletics Data Analysis") + 
  scale_y_continuous(breaks = seq(-1, 1, 0.5),
                     labels = c("100%\nallocated to Men's\nathletics", "50%",
                                "0%",
                                "50%", "100%\nallocated to Women's\nathletics"),
                     expand = expansion(mult = c(0.1,0.05))) + 
  scale_x_discrete(expand = expansion(mult = c(0.025,0.025))) +
  theme(legend.position = "none",
        panel.background = bkg,
        plot.background = bkg,
        panel.grid = element_blank(),
        axis.text.x = element_text(color = fg_col,
                                   family = base_font,
                                   size = 9),
        axis.text.y = element_blank(),
        axis.title.x = element_text(color = fg_col,
                                    family = title_font,
                                    hjust = 0.53),
        axis.title.y = element_blank(),
        plot.margin = ggplot2::margin(t = 20,
                                      l = 10,
                                      r = 20,
                                      b = 5,
                                      unit = "pt"),
        plot.title = ggtext::element_markdown(color = "white",
                                              size = 24,
                                              family = title_font),
        plot.subtitle = ggtext::element_markdown(color = fg_col,
                                                 size = 12,
                                                 family = title_font,
                                                 margin = ggplot2::margin(b = 10,
                                                                          unit = "pt")),
        plot.caption = ggtext::element_markdown(color = fg_col,
                                                family = title_font,
                                                margin = ggplot2::margin(t = 20,
                                                                         unit = "pt")))


ggsave(plot = last_plot(),
       filename = "2022/week_13/sports.png")
