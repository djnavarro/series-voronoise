library(ggplot2)
library(dplyr)
library(ggforce)
library(ambient)
library(tibble)
library(paletteer)

settings <- list(
  seed = 2,
  points = 500, 
  palette = "scico::vik"
)

set.seed(settings$seed)

dat <- tibble(
  x = runif(settings$points),
  y = runif(settings$points),
  z = runif(settings$points)
)

bg <- "black"
exf <- 0

pic <- ggplot(dat, aes(x, y, fill = z)) + 
  geom_voronoi_tile(
    alpha = 1,
    show.legend = FALSE, 
    radius = .01, 
    expand = -.002,
  ) + 
  scale_fill_paletteer_c(settings$palette) +
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg)) + 
  coord_fixed(xlim = c(0,1), ylim = c(0,1))

blt <- ggplot_build(pic)
dat <- as_tibble(blt$data[[1]]) %>% 
  mutate(t = as.factor(fill) %>% as.numeric())
  #mutate(t = as.numeric(stringr::str_remove_all(group, ":.*$")))

pic <- ggplot(
  data = dat,
  mapping = aes(
    x = x,
    y = y,
    fill = fill,
    group = group
  )
) + 
  geom_shape(
    radius = .01,
    expand = -.002,
      show.legend = FALSE) + 
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  scale_fill_identity() + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg)) + 
  coord_fixed(xlim = c(0,1), ylim = c(0,1))

anim <- pic + 
  transition_time(t) + 
  shadow_mark()

anim_save("~/Desktop/vc20.gif", anim, 
          width = 1000, height = 1000, nframes = 368)

