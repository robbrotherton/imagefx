library(tidyverse)
library(imager)

distance <- function(x1, y1, x2, y2) {
  sqrt((x1-x2)^2 + (y1 - y2)^2)
}

area <- function(x1,x2,x3,y1,y2,y3) {
  abs((x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2))/2)
}

# params ------------------------------------------------------------------

file <- "portrait/RB-square.jpg" # "audubon/plate-156-american-crow.jpg"
downsize <- 30 # as a percent of original, e.g. 10%. 100 keeps original
n <- 5000
sd = 1
sides <- 4
# downsize to make it quicker to work with

img <- load.image(file) %>%
  resize(size_x = -downsize, size_y = -downsize)

# plot(img,main="Thumbnail")

img <- img %>%
  grayscale()

img %>%
  cannyEdges(alpha = 0.05) %>%
  as.data.frame() %>%
  ggplot() +
  geom_point(mapping = aes(x, -y))

ed <- img %>%
  cannyEdges(alpha = 0.6) %>%
  as.data.frame() %>%
  sample_n(n()*.8)

ed2 <- img %>%
  cannyEdges(alpha = 0.4) %>%
  as.data.frame() %>%
  sample_n(n()*.25)

ed3 <- img %>%
  cannyEdges(alpha = 0.2) %>%
  as.data.frame() %>%
  sample_n(n()*.25)

ed4 <- img %>%
  cannyEdges(alpha = 0.1) %>%
  as.data.frame() %>%
  sample_n(n()*.25)

ed5 <- img %>%
  cannyEdges(alpha = 0.01) %>%
  as.data.frame() %>%
  sample_n(n()*.1)

img_bw <- img %>%
  as.data.frame() %>%
  filter(value < .9) %>%
  mutate(size = 1-value) %>%
  sample_n(n()*.01, weight = size)

ggplot() +
  geom_point(data = ed,
             mapping = aes(x, -y), size = 1) +
  geom_point(data = ed2,
             mapping = aes(x, -y, size = .8)) +
  geom_point(data = ed3,
             mapping = aes(x, -y, size = .8)) +
  geom_point(data = ed4,
             mapping = aes(x, -y, size = .8)) +
  geom_point(data = ed5,
             mapping = aes(x, -y, size = .8)) +
  # geom_point(data = img_bw,
  #            mapping = aes(x, -y, size = size)) +
  scale_size_identity() +
  coord_fixed() +
  theme_void()

# ggsave("pigeon_points.png",
#        width = 10,
#        height = 10)
