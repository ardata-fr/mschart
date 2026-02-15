#' @export
#' @title Modify axis and plot labels
#' @description Add labels to a chart, labels can be specified for
#' x axis, y axis and plot.
#' @param x an `ms_chart` object.
#' @param title title of the chart (displayed above the plot area). Use NULL to remove it.
#' @param xlab label for the x axis. Use NULL to remove it.
#' @param ylab label for the y axis. Use NULL to remove it.
#' @return An `ms_chart` object.
#' @seealso [chart_data_labels()], [chart_ax_x()], [chart_ax_y()]
#' @examples
#' mylc <- ms_linechart(
#'   data = browser_ts, x = "date", y = "freq",
#'   group = "browser"
#' )
#' mylc <- chart_labels(mylc,
#'   title = "my title", xlab = "my x label",
#'   ylab = "my y label"
#' )
chart_labels <- function(x, title = NULL, xlab = NULL, ylab = NULL) {
  if (!is.null(title)) {
    x$labels[["title"]] <- title
  } else {
    x$labels[["title"]] <- NULL
  }

  if (!is.null(xlab)) {
    x$labels[["x"]] <- xlab
  } else {
    x$labels[["x"]] <- NULL
  }

  if (!is.null(ylab)) {
    x$labels[["y"]] <- ylab
  } else {
    x$labels[["y"]] <- NULL
  }
  x
}
