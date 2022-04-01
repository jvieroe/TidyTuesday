x_location <- (nrow(exp_gap)/2) + 1.5
x_location <- 0 - 1.5
arrow_spec <- arrow(length = unit(0.02, "npc"))


geom_segment(aes(x = x_location, xend = x_location,
                   y = 0, yend = -.75),
               size = .25,
               color = fg_col,
               arrow = arrow_spec) +
  geom_segment(aes(x = x_location, xend = x_location,
                   y = 0, yend = .75),
               size = .25,
               color = fg_col,
               arrow = arrow_spec)