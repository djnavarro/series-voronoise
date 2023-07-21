library(ggplot2)
library(dplyr)
library(ggforce)
library(ambient)
library(tibble)
library(paletteer)

settings <- list(
  seed = 1,
  points = 30, 
  palette = "viridis::magma"
)

set.seed(settings$seed)

dat <- tibble(
  x = runif(settings$points),
  y = runif(settings$points),
  z = runif(settings$points)
)

noise <- .1

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

bg <- "antiquewhite2"
exf <- .1

pic <- ggplot(dat, aes(x, y, fill = z)) + 
  geom_voronoi_tile(
    mapping = aes(x, y, fill = NULL),
    show.legend = FALSE, 
    radius = .01, 
    expand = -.002, 
    fill = "antiquewhite3"
  ) + 
  geom_voronoi_tile(
    stat = "voronoi_tile_noise",
    show.legend = FALSE, 
    radius = .01, 
    expand = -.002,
    alpha = .8
  ) + 
  scale_fill_paletteer_c(settings$palette) +
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg))
  coord_fixed()

ggsave(
  filename = "~/Desktop/vc4.png", 
  plot = pic, 
  width = 5000/300, 
  height = 5000/300,
  dpi = 300
)