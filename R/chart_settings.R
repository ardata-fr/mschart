#' @export
#' @title set chart options
#' @description Set chart properties.
#' @param x an \code{ms_chart} object.
#' @param ... unused parameter
#' @seealso [ms_barchart()], [ms_areachart()], [ms_scatterchart()], [ms_linechart()]
#' @examples
#' chart_01 <- ms_barchart(
#'   data = browser_data, x = "browser",
#'   y = "value", group = "serie"
#' )
#' chart_01 <- chart_settings(
#'   x = chart_01, dir = "vertical",
#'   grouping = "clustered", gap_width = 50
#' )
#'
#' chart_02 <- ms_areachart(data = browser_ts, x = "date",
#'   y = "freq", group = "browser")
#' chart_02 <- chart_settings(chart_02,
#'   grouping = "percentStacked")
chart_settings <- function( x, ... ){
  UseMethod("chart_settings")
}



barchart_options <- function( vary_colors = FALSE, gap_width = 150,
                              dir = "vertical", grouping = "clustered",
                              overlap = 0 ){
  # bardir <- structure(c("bar", "col"), .Names = c("horizontal", "vertical"))
  bardir <- c("horizontal", "vertical")
  if( !dir %in% bardir ){
    stop("dir should be one of ", paste0(shQuote(bardir), collapse = ", " ))
  }

  if( !(gap_width >= 0 && gap_width <= 500) ){
    stop("gap_width should be between 0 and 500")
  }
  if( !grouping %in% st_bargrouping ){
    stop("grouping should be one of ", paste0(shQuote(st_bargrouping), collapse = ", " ))
  }
  if( !(overlap >= -100 && overlap <= 100) ){
    stop("overlap should be between -100 and 100")
  }

  out <- list(vary_colors=vary_colors, gap_width = gap_width, dir = dir, grouping = grouping, overlap = overlap )
  class(out) <- "barchart_options"
  out
}


#' @export
#' @describeIn chart_settings barchart settings
#' @param vary_colors if \code{TRUE} the data points in the single series are displayed the same color.
#' @param gap_width A gap appears between the bar or clustered bars for each category on a bar chart.
#' The default width for this gap is 150 percent of the bar width. It can be set
#' between 0 and 500 percent of the bar width.
#' @param dir the direction of the bars in the chart, value must one of "horizontal" or "vertical".
#' @param grouping grouping for a barchart, a linechart or an area chart. must be one of "percentStacked", "clustered", "standard" or "stacked".
#' @param overlap In a bar chart having two or more series, the bars for each
#' category are clustered together. By default, these bars are directly
#' adjacent to each other. The bars can be made to overlap each other or
#' have a space between them using the overlap property. Its values range
#' between -100 and 100, representing the percentage of the bar width by
#' which to overlap adjacent bars. A setting of -100 creates a gap of a
#' full bar width and a setting of 100 causes all the bars in a category
#' to be superimposed. The default value is 0.
chart_settings.ms_barchart <- function( x, vary_colors,
                                        gap_width, dir, grouping, overlap, ... ){
  options <- barchart_options( vary_colors = ifelse(missing(vary_colors), x$options$vary_colors, vary_colors),
                           gap_width = ifelse(missing(gap_width), x$options$gap_width, gap_width),
                           dir = ifelse(missing(dir), x$options$dir, dir),
                           grouping = ifelse(missing(grouping), x$options$grouping, grouping),
                           overlap = ifelse(missing(overlap), x$options$overlap, overlap) )
  x$options <- options
  x
}


linechart_options <- function( vary_colors = FALSE ){

  out <- list(vary_colors = vary_colors, grouping = "standard" )
  class(out) <- "linechart_options"
  out
}

#' @export
#' @describeIn chart_settings linechart settings
chart_settings.ms_linechart <- function( x, vary_colors, ... ){

  options <- linechart_options( vary_colors = ifelse(missing(vary_colors), x$options$vary_colors, vary_colors) )
  x$options <- options
  x
}




#' @export
#' @describeIn chart_settings linechart settings
chart_settings.ms_areachart <- function( x, vary_colors = FALSE, grouping = "standard", ... ){
  if( !grouping %in% st_grouping ){
    stop("grouping should be one of ", paste0(shQuote(st_grouping), collapse = ", " ))
  }
  options <- list(vary_colors = vary_colors, grouping = grouping )
  class(options) <- "areachart_options"

  x$options <- options
  x
}

#' @export
#' @describeIn chart_settings linechart settings
#' @param scatterstyle The Style for the scatter chart. One
#' of 'none', 'line', 'lineMarker', 'marker', 'smooth', 'smoothMarker'.
chart_settings.ms_scatterchart <- function( x, vary_colors = FALSE, scatterstyle = "lineMarker", ... ){

  if( !scatterstyle %in% st_scatterstyle ){
    stop("scatterstyle should be one of ", paste0(shQuote(st_scatterstyle), collapse = ", " ))
  }
  options <- list(vary_colors = vary_colors, scatterstyle = scatterstyle )
  class(options) <- "scatterchart_options"

  x$options <- options
  x
}
