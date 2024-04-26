#' Apply ggplot2 theme
#'
#' A theme that approximates the style of ggplot2::theme_grey.
#'
#' @param x a mschart object
#' @param base_size base font size
#' @param base_family font family
#'
#' @return a mschart object
#'
#' @export
#'
#' @section theme_ggplot2():
#'
#' \if{html}{\figure{fig_theme_ggplot2.png}{options: width="500"}}
#'
#' @examples
#' p <- ms_scatterchart(
#'   data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width", group = "Species"
#' )
#'
#' p <- theme_ggplot2(p)
#' p <- chart_fill_ggplot2(p)
theme_ggplot2 <- function(x, base_size = 11, base_family = "Arial") {
  t <- mschart_theme(
    main_title = fp_text(color = "black", font.size = 1.2 * base_size, font.family = base_family),
    axis_title = fp_text(color = "black", font.size = base_size, font.family = base_family),
    axis_text = fp_text(color = "grey30", font.size = .8 * base_size, font.family = base_family),
    axis_ticks = fp_border(color = "grey20", width = 1, style = "solid"),
    grid_major_line_x = fp_border(color = "white", width = 1, style = "solid"),
    grid_major_line_y = fp_border(color = "white", width = 1, style = "solid"),
    grid_minor_line_x = fp_border(color = "white", width = .5, style = "solid"),
    grid_minor_line_y = fp_border(color = "white", width = .5, style = "solid"),
    chart_background = "white",
    plot_background = "grey92",
    legend_text = fp_text(color = "black", font.size = base_size, font.family = base_family),
    legend_position = "r"
  )
  set_theme(x, t)
}

#' Apply ggplot2 color scale
#'
#' The default hue color scale from ggplot2.
#'
#' @param x a mschart object
#' @param stroke a boolean. Apply the color scale to stroke? Defaults to `TRUE`.
#'
#' @return a mschart object
#'
#' @export
#'
#' @section chart_fill_ggplot2():
#'
#' \if{html}{\figure{fig_theme_ggplot2.png}{options: width="500"}}
#'
#' @examples
#' p <- ms_scatterchart(
#'   data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width", group = "Species"
#' )
#'
#' p <- theme_ggplot2(p)
#' p <- chart_fill_ggplot2(p)
#' @importFrom scales hue_pal
chart_fill_ggplot2 <- function(x, stroke = TRUE) {
  if (!is.null(x$group)) {
    groups <- unique(x$data[[x$group]])
    ngroups <- length(groups)
    pal <- hue_pal()(ngroups)
    names(pal) <- groups
  } else {
    pal <- hue_pal()(1)
  }

  x <- chart_data_fill(x, values = pal)
  if (stroke) x <- chart_data_stroke(x, values = pal)
  x
}
