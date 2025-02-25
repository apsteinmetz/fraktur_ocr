library(ggplot2)

GeomVerticalBand <- ggproto(
  "GeomVerticalBand",
  Geom,
  required_aes = c("xmin", "xmax", "label"),
  default_aes = aes(
    y = 0.5,
    label = "",
    alpha = 0.2,
    fill = "gray",
    colour = NA,
    label_color = NA,
    size = 0.5,
    linetype = "solid"
  ),
  draw_panel = function(data, panel_params, coord) {
    data <- coord$transform(data, panel_params)

    grid::gList(
      grid::rectGrob(
        x = (data$xmin + data$xmax) / 2,
        y = 0.5,
        width = abs(data$xmax - data$xmin),
        height = unit(1, "npc"),
        default.units = "native",
        just = "center",
        gp = grid::gpar(
          fill = scales::alpha(data$fill, data$alpha),
          col = scales::alpha(data$colour, data$alpha),
          lwd = data$size,
          lty = data$linetype
        )
      ),
      grid::textGrob(
        label = data$label,
        x = (data$xmin + data$xmax) / 2,
        y = data$y,
        default.units = "native",
        gp = grid::gpar(
          col = data$label_color
        )
      )
    )
  }
)


# roxygen2 documentation
#' @importFrom ggplot2 ggproto Geom
#' @importFrom scales alpha
#' @importFrom grid gList rectGrob textGrob gpar
#' @export
#' @rdname geom_vertical_band
#' @title Vertical band
#' @description Add a vertical band to a plot.
#' @param xmin, xmax The x-axis limits of the band.
#' @param label_y The y-axis position of the label.
#' @param label The label to display.
#' @param fill The fill color of the band.
#' @param alpha The transparency of the band.
#' @examples
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' ggplot(df, aes(x, y)) +
#'  geom_line() +
#'  geom_vertical_band(xmin = 3, xmax = 5, label_y = 1, label = "Band 1", fill = "blue", alpha = 0.1) +
#'  geom_vertical_band(xmin = 7, xmax = 9, label_y = 1, label = "Band 2", fill = "red", alpha = 0.1)
#'
geom_vertical_band <- function(
  xmin,
  xmax,
  label_y = 0.5,
  label = "",
  fill = "gray",
  alpha = 0.2,
  label_color = "black",
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  color = NA,
  size = 0.5,
  linetype = "solid",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    geom = GeomVerticalBand,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmin = xmin,
      xmax = xmax,
      y = label_y,
      label = label,
      fill = fill,
      alpha = alpha,
      label_color = label_color,
      colour = color,
      size = size,
      linetype = linetype,
      na.rm = na.rm,
      ...
    )
  )
}

# The  geom_vertical_band  function is a ggplot2 geom that adds a vertical band to a plot. The band is defined by the  xmin  and  xmax  arguments, and the label is placed at the  label_y  position. The fill color and transparency of the band can be customized.
# The function is defined using the ggproto function, which creates a new ggplot2 geom object. The  draw_panel  method is used to draw the band and label on the plot.
# The function is exported so that it can be used in other R scripts.
# To use the  geom_vertical_band  function, you can include it in your R script and then call it within a ggplot2 plot. For example:
df <- data.frame(x = 1:10, y = rnorm(10))
ggplot(df, aes(x, y)) +
  geom_line() +
  geom_vertical_band(
    xmin = 3,
    xmax = 5,
    label_y = 0,
    label = "Band 1",
    fill = "blue",
    alpha = 0.1,
    label_color = "green"
  ) +
  geom_vertical_band(
    xmin = 7,
    xmax = 9,
    label_y = 1,
    label = "Band 2",
    fill = "red",
    alpha = 0.1,
    color = "black"
  )
