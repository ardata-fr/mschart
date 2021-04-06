#' @export
#' @title Modify axis and plot labels
#' @description Add labels to a chart, labels can be specified for
#' x axis, y axis and plot.
#' @param x an \code{ms_chart} object.
#' @param title,xlab,ylab Text to add
#' @examples
#' mylc <- ms_linechart(data = browser_ts, x = "date", y = "freq",
#'   group = "browser")
#' mylc <- chart_labels(mylc, title = "my title", xlab = "my x label",
#'   ylab = "my y label")
chart_labels <- function( x, title = NULL, xlab = NULL, ylab = NULL){
  if( !is.null(title) ) x$labels[["title"]] <- htmlEscape(title)
  else x$labels[["title"]] <- NULL

  if( !is.null(xlab) ) x$labels[["x"]] <- htmlEscape(xlab)
  else x$labels[["x"]] <- NULL

  if( !is.null(ylab) ) x$labels[["y"]] <- htmlEscape(ylab)
  else x$labels[["y"]] <- NULL
  x
}
