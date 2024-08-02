# # library(tidyverse)
# # library(imager)
#
#
# # params ------------------------------------------------------------------
#
# downsize <- 20
#
# # setup -------------------------------------------------------------------
# # list.files("audubon")
# file <- "audubon/plate-168-fork-tailed-flycatcher.jpg" # "audubon/pigeon.jpg"
#
# img <- load.image(file) %>%
#   resize(-downsize, -downsize)
#
# img_col <- img %>%
#   as.data.frame(wide = "c") %>%
#   mutate(rgb = rgb(c.1,c.2,c.3)) %>%
#   rowwise() %>%
#   mutate(rgb_val = sum(c.1,c.2,c.2)) %>%
#   ungroup()
#
# # find black/white areas
# img_g <- img %>% grayscale()
# img_f <- img_g %>% ecdf()
# img_e <- img_f(img_g) %>% as.cimg(dim = dim(img_g))
#
# img_bw <- img_g %>%
#   threshold(.7) %>%
#   as.data.frame()
#
# draw(img_bw)
#
# l <- img_bw %>%
#   group_by(x) %>%
#   arrange(y, .by_group = T) %>%
#   rowid_to_column("id") %>%
#   mutate(discon = lead(y) == y + 1) %>%
#   ungroup()
#
# # make it long
# # starts <- filter(l, discon==F) %>% mutate(y = y + 1)
# # ends <- filter(l, id %in% (starts$id + 1))
# # start_stop <- bind_rows(starts, ends) %>%
# #   arrange(id)
#
# # make it wide
# starts <- filter(l, discon==F) %>% mutate(y_start = y + 1)
# ends <- filter(l, id %in% (starts$id + 1)) %>%
#   mutate(y_end = y) %>%
#   select(y_end)
#
# start_stop <- bind_cols(starts, ends) %>%
#   arrange(id) %>%
#   select(x, y_start, y_end) %>%
#   rowid_to_column("section")
#
# img_l <- img_col %>%
#   group_by(x) %>%
#   group_split()
#
# for(i in 1:max(start_stop$section)) {
#
#   # identify the rows to reverse
#   rows <- start_stop$y_start[i]:start_stop$y_end[i]
#
#   # identify the x column
#   x <- start_stop$x[i]
#
#   vals <- img_l[[x]][rows,c("rgb","rgb_val")]
#   # vals <- arrange(vals, desc(rgb_val)) # light to dark
#   vals <- arrange(vals, rgb_val) # dark to light
#   # vals$rgb <- sample(vals$rgb, size = length(vals$rgb)) # just random; doesn't look good
#   # img_l[[start_stop$x[i]]][start_stop$y_start[i]:start_stop$y_end[i],]
#
#   img_l[[x]][rows,"rgb"] <- vals$rgb
# }
#
#
# pic <- img_l %>%
#   bind_rows() %>%
#   ggplot() +
#   geom_raster(aes(x, -y, fill = rgb)) +
#   scale_fill_identity() +
#   coord_fixed() +
#   theme_void() +
#   # theme(panel.background = element_rect(fill = hsv(0,.05,.5))) +
#   NULL
#
# plot(pic)
#
# # ggsave(paste0(file, "_glitch.png"),
# #        width = 10,
# #        height = 10)
