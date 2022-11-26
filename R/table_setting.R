#' @title x table settings
#' @description Define settings for an x table.
#' @param x an `ms_chart` object.
#' @param horizontal write horizontal lines in the table
#' @param vertical write vertical lines in the table
#' @param outline write an outline in the table
#' @param show_keys showkeys in the table
#'
#' @export
chart_table <- function(x, horizontal,
                        vertical,
                        outline,
                        show_keys) {
  stopifnot(inherits(x, "ms_chart"))

  options <- list(
    horizontal = ifelse(missing(horizontal), x$x_table$horizontal, horizontal),
    vertical = ifelse(missing(vertical), x$x_table$vertical, vertical),
    outline = ifelse(missing(outline), x$x_table$outline, outline),
    show_keys = ifelse(missing(show_keys), x$x_table$show_keys, show_keys)
  )

  x$x_table <- do.call(table_options, options)
  x
}


table_options <- function(horizontal = TRUE, vertical = TRUE,
                          outline = TRUE, show_keys = TRUE) {
  if (!is.logical(horizontal)) {
    stop("horizontal should be of type logical")
  }

  if (!is.logical(vertical)) {
    stop("vertical should be of type logical")
  }

  if (!is.logical(outline)) {
    stop("outline should be of type logical")
  }

  if (!is.logical(show_keys)) {
    stop("show_keys should be of type logical")
  }

  out <- list(
    horizontal = horizontal,
    vertical = vertical,
    outline = outline,
    show_keys = show_keys
  )

  class(out) <- "table_options"
  out
}
