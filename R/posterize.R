
library(imager)
library(ggplot2)
# params ------------------------------------------------------------------

# multiplier <- 8
# red <- 1
# lines <- 80
# x_freq <- .5
# w_point <- 1

sd <- 10
downsize <- 20

# setup -------------------------------------------------------------------

file <- "/Users/rbrother/Desktop/plate-168-fork-tailed-flycatcher.jpg"
img <- load.image(file) |>
  resize(-downsize, -downsize)

posterize <- function(img) {
  img_df <- img |>
    as.data.frame(wide = "c") |>
    dplyr::mutate(rgb = rgb(c.1,c.2,c.3))

  img2 <- img |>
    grayscale() |>
    as.data.frame() |>
    dplyr::filter(value < .75)
}

# helpers -----
rhsv <- function(h = runif(1), s = runif(1), v = runif(1)) {
  hsv(h, s, v)
}

r <- function(sd) {
  rnorm(1, sd = sd)
}

pic <- ggplot(img2) +
  geom_tile(aes(x, -y), fill = 'black', alpha = 1, color = NA) +
  geom_tile(aes(x+r(sd), -y+r(sd)), fill = rhsv(), alpha = .7, color = NA) +
  geom_tile(aes(x+r(sd), -y+r(sd)), fill = rhsv(), alpha = .7, color = NA) +
  geom_tile(aes(x+r(sd), -y+r(sd)), fill = rhsv(), alpha = .7, color = NA) +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = rhsv()))

plot(pic)


# CMYK
img1 <- img_df |>
  dplyr::filter(!stringr::str_detect(img_df$rgb, "#FFFFFF"))

pic <- ggplot() +
  geom_tile(data = img2,
            mapping = aes(x+r(sd), -y+r(sd)), fill = "cyan", alpha = .2, color = NA) +
  geom_tile(data = img2,
            mapping = aes(x+r(sd), -y+r(sd)), fill = "magenta", alpha = .2, color = NA) +
  geom_tile(data = img2,
            mapping = aes(x+r(sd), -y+r(sd)), fill = "yellow", alpha = .2, color = NA) +
  # geom_raster(data = img1,
  #             mapping = aes(x, -y, fill = rgb)) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  # theme(panel.background = element_rect(fill = rhsv())) +
  NULL

plot(pic)
