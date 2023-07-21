library(ggplot2)
library(dplyr)
library(ggforce)
library(ambient)
library(tibble)
library(paletteer)

settings <- list(
  seed = 2,
  points = 300, 
  palette = "scico::grayC"
)

set.seed(settings$seed)

dat <- tibble(
  x = runif(settings$points),
  y = runif(settings$points),
  z = runif(settings$points)
)

StatVoronoiTileNoise <- ggproto("StatVoronoiTileNoise", StatVoronoiTile,
    compute_group = function(data, scales, bound = NULL, eps = 1e-09, 
                             max.radius = NULL, normalize = FALSE, asp.ratio = 1) {
      
      vtile <- StatVoronoiTile$compute_group(
        data, scales, bound, eps, max.radius, 
        normalize, asp.ratio)
      
      vtile <- vtile %>% 
        group_by(group) %>% 
        mutate(
          x = x * 1.2, 
          y = y * 1.2
        ) %>% 
        ungroup()
      
      return(vtile)
    }                            
)

bg <- "hotpink"
exf <- 0

pic <- ggplot(dat, aes(x, y, fill = z)) + 
  geom_voronoi_tile(
    alpha = .5,
    show.legend = FALSE, 
    radius = .01, 
    expand = -.002,
  ) + 
  geom_voronoi_tile(
    mapping = aes(x, y, fill = NULL),
    fill = "black",
    stat = "voronoi_tile_noise",
    show.legend = FALSE, 
    radius = .01, 
    expand = -.002,
    alpha = .5
  ) + 
  scale_fill_paletteer_c(settings$palette) +
  scale_x_continuous(expand = c(1,1) * exf) + 
  scale_y_continuous(expand = c(1,1) * exf) + 
  theme_void() +
  theme(panel.background = element_rect(fill = bg, colour = bg)) + 
  coord_fixed(xlim = c(0,1), ylim = c(0,1))

ggsave(
  filename = "~/Desktop/vc17.png", 
  plot = pic, 
  width = 5000/300, 
  height = 5000/300,
  dpi = 300
)