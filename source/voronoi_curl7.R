library(ggplot2)
library(dplyr)
library(ggforce)
library(ambient)
library(tibble)

settings <- list(
  seed = 1,
  points = 100
)

set.seed(settings$seed)

fill_f <- jasmines::palette_named("blood")
fill_b <- jasmines::palette_named("ropensci")

cols_f <- fill_f(settings$points)
cols_b <- fill_b(settings$points)

dat <- tibble(
  x = runif(settings$points),
  y = runif(settings$points),
  z = runif(settings$points),
  cols_f = sample(cols_f),
  cols_b = sample(cols_b)
)

noise <- .5

StatVoronoiTileNoise <- ggproto("StatVoronoiTileNoise", StatVoronoiTile,
    compute_group = function(data, scales, bound = NULL, eps = 1e-09, 
                             max.radius = NULL, normalize = FALSE, asp.ratio = 1) {
      
      vtile <- StatVoronoiTile$compute_group(
        data, scales, bound, eps, max.radius, 
        normalize, asp.ratio)
      
      vtile <- vtile %>% 
        group_by(group) %>% 
        mutate(
          x = x + runif(1, min = -noise/2, max = noise/2),
          y = y + runif(1, min = -noise/2, max = noise/2)
        ) %>% 
        ungroup()
      
      return(vtile)
    }                            
)

bg <- "honeydew4"
exf <- .1

pic <- ggplot(dat, aes(x, y)) + 
  geom_voronoi_tile(
    mapping = aes(fill = cols_b, group = 1),
    show.legend = FALSE, 
    radius = .01, 
    expand = -.001,
    alpha = 1
  ) + 
  geom_voronoi_tile(
    mapping = aes(fill = cols_f, group = 1),
    stat = "voronoi_tile_noise",
    show.legend = FALSE, 
    radius = .01, 
    expand = -.001,
    alpha = 1
  ) + 
  scale_fill_identity() +
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg))
  coord_fixed()

ggsave(
  filename = "~/Desktop/vc7.png", 
  plot = pic, 
  width = 5000/300, 
  height = 5000/300,
  dpi = 300
)