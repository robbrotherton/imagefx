
#' Read an image file
#'
#' @param file
#' @param resize If the dimension arguments are negative, they are interpreted as a proportion of the original image.
#'
#' @return
#' @export
#'
#' @examples
read_image <- function(file, resize = -100L) {
  imager::load.image(file)  |>
    imager::resize(resize, resize)
}


image_to_df <- function(img) {
  img |>
    as.data.frame(wide = "c") |>
    dplyr::mutate(rgb = rgb(c.1,c.2,c.3)) |>
    rowwise() |>
    mutate(rgb_val = sum(c.1,c.2,c.2)) |>
    ungroup()
}


#' Title
#'
#' @param df
#' @param fill
#'
#' @return
#' @export
#'
#' @examples
render_image <- function(df, fill = NULL) {
  ggplot2::ggplot(df) +
    ggplot2::geom_raster(ggplot2::aes(x, -y, fill = {{fill}})) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
