#' @export
#' @title Set a barchart as a stacked barchart
#' @description Apply settings to an \code{ms_barchart} object to
#' produce a stacked barchart. Options are available to use percentage
#' instead of values and to choose if bars should be vertically or horizontally drawn.
#' @param x an \code{\link{ms_barchart}} object
#' @param dir the direction of the bars in the chart, value must be one of "horizontal" or "vertical".
#' @param percent should bars be displayed as percentages.
#' @param gap_width gap width between bars for each category on a bar chart, as a percentage
#' of the bar width. It can be set between 0 and 500.
#' @return An `ms_chart` object.
#' @seealso [chart_settings()], [ms_barchart()]
#' @examples
#' library(officer)
#'
#' my_bar_stack_01 <- ms_barchart(data = browser_data, x = "browser",
#'   y = "value", group = "serie")
#' my_bar_stack_01 <- as_bar_stack( my_bar_stack_01 )
#'
#' my_bar_stack_02 <- ms_barchart(data = browser_data, x = "browser",
#'   y = "value", group = "serie")
#' my_bar_stack_02 <- as_bar_stack( my_bar_stack_02, percent = TRUE,
#'   dir = "horizontal" )
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
#' doc <- ph_with(doc, my_bar_stack_02, location = ph_location_fullsize())
#'
#' fileout <- tempfile(fileext = ".pptx")
#' print(doc, target = fileout)
as_bar_stack <- function(x, dir = "vertical", percent = FALSE, gap_width = 50){

  stopifnot(inherits(x, "ms_barchart"))

  grouping <- "stacked"
  if( percent ) grouping <- "percentStacked"

  x <- chart_settings( x, grouping = grouping, dir = dir, gap_width = gap_width, overlap = 100 )
  x <- chart_data_stroke( x, values = "transparent" )
  if( dir == "horizontal" )
    x <- chart_theme( x = x, title_x_rot = 270, title_y_rot = 0)

  x
}

