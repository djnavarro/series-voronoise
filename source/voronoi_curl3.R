library(ggplot2)
library(dplyr)
library(ggforce)
library(ambient)
library(tibble)
library(paletteer)

settings <- list(
  seed = 4,
  points = 50, 
  palette = "scico::hawaii"
)

set.seed(settings$seed)

dat <- tibble(
  x = runif(settings$points),
  y = runif(settings$points),
  z = runif(settings$points)
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

bg <- "grey90"
bg2 <- "grey80"
exf <- .1

pic <- ggplot(dat, aes(x, y, fill = z)) + 
  geom_voronoi_tile(
    mapping = aes(x, y, fill = NULL),
    show.legend = FALSE, 
    radius = .01, 
    expand = -.001, 
    fill = bg2
  ) + 
  geom_voronoi_tile(
    stat = "voronoi_tile_noise",
    show.legend = FALSE, 
    radius = .01, 
    expand = -.001,
    alpha = 1
  ) + 
  scale_fill_paletteer_c(settings$palette) +
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg))
  coord_fixed()

ggsave(
  filename = "~/Desktop/vc3.png", 
  plot = pic, 
  width = 5000/300, 
  height = 5000/300,
  dpi = 300
)