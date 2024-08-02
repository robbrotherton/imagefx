library(tidyverse)
# library(magick)
library(imager)

distance <- function(x1, y1, x2, y2) {
  sqrt((x1-x2)^2 + (y1 - y2)^2)
}

# file <- paste0("audubon/", sample(list.files("audubon"), 1))
file <- "audubon/pigeon.jpg"
img <- load.image(file)

thmb <- resize(img,round(width(img)*1),round(height(img)*1))
# plot(thmb,main="Thumbnail")

# imgradient(thmb, "xy") %>%
#   enorm() %>%
#   grayscale() %>%
#   threshold("99%") %>%
#   as.cimg() %>%
#   as.data.frame() %>%
#   sample_n(1000, weight = (value)) %>%
#   select(x, y) -> data

# grayscale
d <- thmb %>%
  imgradient("xy") %>%
  enorm() %>%
  grayscale() %>%
  threshold("80%") %>%
  as.cimg() %>%
  as.data.frame()

ggplot(d,aes(x,-y))+
  geom_raster(aes(fill=value)) +
  coord_fixed()

# USE THIS MASK FOR MAGPIES
mask <- thmb %>%
  as.data.frame(wide = "c") %>%
  rowwise() %>%
  mutate(value = mean(c(c.1, c.2, c.2)),
         rgb.val=rgb(c.1,c.2,c.3)) %>%
  filter(value > .9)

# ALT MASK
# mask <- img %>%
#   imgradient("xy") %>%
#   enorm() %>%
#   grayscale() %>%
#   threshold("50%") %>%
#   as.cimg() %>%
#   as.data.frame() %>%
#   filter(value == 0)

col_img <- thmb %>%
  as.data.frame(wide = "c") %>%
  rowwise() %>%
  mutate(value = mean(c(c.1, c.2, c.2)),
         rgb.val=rgb(c.1,c.2,c.3))



ggplot() +
  geom_raster(data = mask,
              mapping = aes(x, -y))


points <- d %>%
  filter(value > 0) %>%
  sample_n(1500, weight = value) %>%
  select(x, y)

# points <- data.frame(x = points$x, y = points$y) #runif(500), y = runif(500))

del <- deldir::deldir(x = points$x, y = points$y)$delsgs %>%
  mutate(d = distance(x1,y1,x2,y2)) %>%
  mutate(x = round(x1,0),
         y = round(y1,0))

triangles <- deldir::deldir(x = points$x, y = points$y)
triangles <- deldir::triang.list(triangles) %>%
  bind_rows(.id = "group") %>%
  group_by(group) %>%
  mutate(avg_x = round(mean(x), 0),
         avg_y = round(mean(y), ))

del_col <- triangles %>%
  left_join(col_img, by = c("x","y"))

# del_long <- del_col %>%
#   select(x1,x2,y1,y2,rgb.val) %>%
#   mutate(group = 1:n()) %>%
#   pivot_longer(cols = c(x1,x2))

# this list stuff allows calculation of length of each side of triangle
l <- del_col %>%
  group_by(group) %>%
  group_split()
l2 <- map(l, ~rbind(., .[1,]))
l3 <- map_df(l2, ~mutate(., d = distance(.$x, .$y, lead(.$x), lead(.$y)))[1:3,])
del_col <- filter(l3, d < 40)

pic <- ggplot() +
  geom_polygon(data = del_col, mapping = aes(x, y, group = group, fill = rgb.val), color = 'black') +
  # geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2)) +
  # geom_raster(data = mask, mapping = aes(x, y), fill = 'white') +
  # geom_point(data = del_col, mapping = aes(x, y, color = rgb.val)) +
  scale_fill_identity() +
  scale_y_continuous(trans = "reverse", expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'none')

plot(pic)

# ggsave(paste0(file, ".png"),
#        width = width(img)/320,
#        height = height(img)/320)



# using ggforce::geom_delaunay --------------------------------------------

# df <- d %>%
#   sample_n(5000, weight = value) %>%
#   mutate(rgb.r=rgb(round(c.1, 0),round(c.2,0),round(c.3,0)))
#
# ggplot(df) +
#   # geom_point(aes(x = x, y = -y, color=rgb.r), size = 2) +
#   ggforce::geom_delaunay_tile(aes(x, y, fill = rgb.r, color = rgb.r, group = -1L),
#                               alpha = .3, color = 'black') +
#   # scale_color_identity() +
#   coord_fixed() +
#   theme_void()

# using k-nearest-neighbor algorithm --------------------------------------

# k_nearest_neighbour_graph <- function(points, k=8) {
#   get_k_nearest <- function(points, ptnum, k) {
#     xi <- points$x[ptnum]
#     yi <- points$y[ptnum]
#     points %>%
#       dplyr::mutate(dist = sqrt((x - xi)^2 + (y - yi)^2)) %>%
#       dplyr::arrange(dist) %>%
#       dplyr::filter(row_number() %in% seq(2, k+1)) %>%
#       dplyr::mutate(xend = xi, yend = yi)
#   }
#
#   1:nrow(points) %>%
#     purrr::map_df(~get_k_nearest(points, ., k))
# }
#
#
# data <- k_nearest_neighbour_graph(points, k = 8)
#
# ggplot(data) +
#   # geom_point(data = points, mapping = aes(x, y)) +
#   geom_curve(aes(x=x,y=y,xend=xend,yend=yend), curvature = 0) +
#   scale_color_identity() +
#   coord_fixed() +
#   theme_void()
