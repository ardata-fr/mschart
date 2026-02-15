#' @export
#' @title Set chart options
#' @description Set chart properties.
#' @param x an \code{ms_chart} object.
#' @param ... unused parameter
#' @return An `ms_chart` object.
#' @seealso [ms_barchart()], [ms_areachart()], [ms_scatterchart()], [ms_linechart()]
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_chart_settings_1.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_chart_settings_2.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_chart_settings_3.png}{options: width="500"}}
#' @examples
#' library(mschart)
#' library(officer)
#'
#' chart_01 <- ms_barchart(
#'   data = browser_data, x = "browser",
#'   y = "value", group = "serie"
#' )
#' chart_01 <- chart_theme(chart_01,
#'   grid_major_line_x = fp_border(width = 0),
#'   grid_minor_line_x = fp_border(width = 0)
#' )
#'
#' chart_02 <- chart_settings(
#'   x = chart_01,
#'   grouping = "stacked", overlap = 100
#' )
#'
#'
#' chart_03 <- ms_areachart(
#'   data = browser_ts, x = "date",
#'   y = "freq", group = "browser"
#' )
#' chart_03 <- chart_settings(chart_03,
#'   grouping = "percentStacked"
#' )
chart_settings <- function(x, ...) {
  UseMethod("chart_settings")
}


barchart_options <- function(vary_colors = FALSE, gap_width = 150,
                             dir = "vertical", grouping = "clustered",
                             overlap = 0, table = FALSE) {
  # bardir <- structure(c("bar", "col"), .Names = c("horizontal", "vertical"))
  bardir <- c("horizontal", "vertical")
  if (!dir %in% bardir) {
    stop("dir should be one of ", paste0(shQuote(bardir), collapse = ", "))
  }

  if (!(gap_width >= 0 && gap_width <= 500)) {
    stop("gap_width should be between 0 and 500")
  }
  if (!grouping %in% st_bargrouping) {
    stop("grouping should be one of ", paste0(shQuote(st_bargrouping), collapse = ", "))
  }
  if (!(overlap >= -100 && overlap <= 100)) {
    stop("overlap should be between -100 and 100")
  }

  out <- list(vary_colors = vary_colors, gap_width = gap_width, dir = dir, grouping = grouping, overlap = overlap, table = table)
  class(out) <- "barchart_options"
  out
}


#' @export
#' @describeIn chart_settings barchart settings
#' @param vary_colors if \code{TRUE}, each data point in a single series is displayed in a different color.
#' @param gap_width A gap appears between the bar or clustered bars for each category on a bar chart.
#' The default width for this gap is 150 percent of the bar width. It can be set
#' between 0 and 500 percent of the bar width.
#' @param dir the direction of the bars in the chart, value must be one of "horizontal" or "vertical".
#' @param grouping grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlap In a bar chart having two or more series, the bars for each
#' category are clustered together. By default, these bars are directly
#' adjacent to each other. The bars can be made to overlap each other or
#' have a space between them using the overlap property. Its values range
#' between -100 and 100, representing the percentage of the bar width by
#' which to overlap adjacent bars. A setting of -100 creates a gap of a
#' full bar width and a setting of 100 causes all the bars in a category
#' to be superimposed. The default value is 0.
#' @param table if \code{TRUE} set a table below the barchart.
chart_settings.ms_barchart <- function(x, vary_colors,
                                       gap_width, dir, grouping, overlap, table, ...) {
  options <- barchart_options(
    vary_colors = if(missing(vary_colors)) x$options$vary_colors else vary_colors,
    gap_width = if(missing(gap_width)) x$options$gap_width else gap_width,
    dir = if(missing(dir)) x$options$dir else dir,
    grouping = if(missing(grouping)) x$options$grouping else grouping,
    overlap = if(missing(overlap)) x$options$overlap else overlap,
    table = if(missing(table)) x$options$table else table
  )
  x$options <- options
  x
}


linechart_options <- function(vary_colors = FALSE, table = FALSE) {
  out <- list(vary_colors = vary_colors, grouping = "standard", table = table)
  class(out) <- "linechart_options"
  out
}

#' @export
#' @describeIn chart_settings linechart settings
#' @param style Style for the linechart or scatterchart type of markers. One
#' of 'none', 'line', 'lineMarker', 'marker', 'smooth', 'smoothMarker'.
chart_settings.ms_linechart <- function(x, vary_colors, style, table, ...) {
  options <- linechart_options(
    vary_colors = if(missing(vary_colors)) x$options$vary_colors else vary_colors,
    table = if(missing(table)) x$options$table else table
  )

  style <- if(missing(style)) x$options$linestyle %||% "lineMarker" else style
  if (!style %in% st_scatterstyle) {
    stop("style should be one of ", paste0(shQuote(st_scatterstyle), collapse = ", "))
  }

  options$linestyle <- style
  x$options <- options
  x
}




#' @export
#' @describeIn chart_settings areachart settings
chart_settings.ms_areachart <- function(x, vary_colors, grouping, table, ...) {
  vary_colors <- if(missing(vary_colors)) x$options$vary_colors %||% FALSE else vary_colors
  grouping <- if(missing(grouping)) x$options$grouping %||% "standard" else grouping
  table <- if(missing(table)) x$options$table %||% FALSE else table

  if (!grouping %in% st_grouping) {
    stop("grouping should be one of ", paste0(shQuote(st_grouping), collapse = ", "))
  }
  options <- list(vary_colors = vary_colors, grouping = grouping, table = table)
  class(options) <- "areachart_options"

  x$options <- options
  x
}

#' @export
#' @describeIn chart_settings scatterchart settings
chart_settings.ms_scatterchart <- function(x, vary_colors, style, ...) {
  vary_colors <- if(missing(vary_colors)) x$options$vary_colors %||% FALSE else vary_colors
  style <- if(missing(style)) x$options$scatterstyle %||% "marker" else style

  if (!style %in% st_scatterstyle) {
    stop(
      "style should be one of ",
      paste0(shQuote(st_scatterstyle), collapse = ", ")
    )
  }

  if (grepl("smooth", style)) {
    x <- chart_data_smooth(x, values = 1)
  } else {
    x <- chart_data_smooth(x, values = 0)
  }

  options <- list(vary_colors = vary_colors, scatterstyle = style, table = FALSE)
  class(options) <- "scatterchart_options"

  x$options <- options
  x
}


piechart_options <- function(vary_colors = TRUE, hole_size = 0) {
  if (!(hole_size >= 0 && hole_size <= 90)) {
    stop("hole_size should be between 0 and 90")
  }
  out <- list(vary_colors = vary_colors, hole_size = hole_size)
  class(out) <- "piechart_options"
  out
}

#' @export
#' @describeIn chart_settings piechart settings
#' @param hole_size size of the hole in a doughnut chart, between 0 and 90
#' (percent of the radius). Default 0 produces a pie chart;
#' values above 0 produce a doughnut chart.
chart_settings.ms_piechart <- function(x, vary_colors, hole_size, ...) {
  options <- piechart_options(
    vary_colors = if (missing(vary_colors)) x$options$vary_colors else vary_colors,
    hole_size = if (missing(hole_size)) x$options$hole_size else hole_size
  )
  x$options <- options
  x
}
