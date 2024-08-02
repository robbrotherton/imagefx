#' Title
#'
#' @param img
#' @param n_lines
#' @param amplitude
#' @param frequency
#' @param white_point
#'
#' @return
#' @export
#'
#' @examples
image_to_sine <- function(img, n_lines = 25, amplitude = 6, frequency = 1, white_point = 0.5) {

  line_positions <- round(seq(from = 1,
                              to = imager::height(img),
                              length.out = n_lines), 0)

  img_df <- img |>
      imager::grayscale() |>
      as.data.frame() |>
      dplyr::mutate(value = ifelse(value > white_point, pmin(1, value+(value-white_point)*2), value))

    img2 <- img_df |>
      dplyr::mutate(y0 = -y + sin(x*frequency) * (1-value) * amplitude,
             c = value <= .99) |>
      dplyr::mutate(color = runif(1, 1, 10), .by = y) |>
      dplyr::filter(y %in% line_positions)

    img2

}



# spiral  ------------------------------------------------------------------
# w <- width(img)
# h <- height(img)
#
#
# Rcpp::cppFunction(
#   'DataFrame spiral(int coils, int points, double radius, double inner_radius, NumericVector dist) {
#   #include <cmath>
#             // create the columns
#             double n = coils * points;
#             double awayStep = radius/n;
#             double aroundStep = coils/n;
#             double aroundRadians = aroundStep * 2 * M_PI;
#             NumericVector x(n);
#             NumericVector y(n);
#             for(int i = 0; i < n; ++i) {
#             double away = awayStep * i + inner_radius;
#             double away0 = away + dist[i];
#             double around = i * aroundRadians;
#             x[i] = cos(around) * away0;
#             y[i] = sin(around) * away0;
#             }
#             // return a new data frame
#             return DataFrame::create(_["x"]= x, _["y"]= y);
#             }
#             '
# )
#
# s <- spiral(coils = lines, points = w, radius = h*.55, inner_radius = 0, dist = 0) %>%
#   mutate(x0 = x + w/2,
#          y0 = y + h/2,
#          x = round(x0, 0),
#          y = round(y0, 0),
#          coil = rep(1:lines, each = w),
#          x_seq = rep(1:w, length.out = n())) %>%
#   left_join(img_df) %>%
#   replace_na(list(value = 1)) %>%
#   mutate(value = 1 - value,
#          sine = sin(x_seq) * value * multiplier)
#
# sp <- spiral(coils = lines, points = w, radius = h*.55, inner_radius = 0, dist = s$sine*.8)
#
# pic <- ggplot() +
#   # geom_raster(data = img_df, aes(x, y, fill = value)) +
#   geom_path(data = sp,
#             mapping = aes(x = x, y = -y)) +
#   coord_fixed() +
#   theme_void()
#
# plot(pic)

# img <- resize(img,round(width(img)*.1),round(height(img)*.1))


# img2 <- img_df %>%
#   mutate(y0 = -y + sin(x*x_freq) * (1-value) * multiplier,
#          c = value <= .99) %>%
#   filter(y %in% line_positions)
#
#
# pic <- ggplot() +
#   geom_path(data = img2,
#             mapping = aes(x, y0, group = y, color = y==red_line)) +
#   scale_color_manual(values = c("black","red")) +
#   coord_fixed() +
#   theme_void() +
#   theme(panel.background = element_rect(fill = hsv(.06,.04,1),
#                                         color = 'black',
#                                         size = 4),
#         legend.position='none')
#
# plot(pic)

