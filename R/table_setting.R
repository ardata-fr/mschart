#' @title Data table settings
#' @description Define visual settings for the data table displayed below the
#' chart. Requires `chart_settings(x, table = TRUE)` to be called first.
#' @param x an `ms_chart` object.
#' @param horizontal write horizontal lines in the table
#' @param vertical write vertical lines in the table
#' @param outline write an outline in the table
#' @param show_keys show keys in the table
#' @return An `ms_chart` object.
#' @seealso [chart_settings()]
#' @export
#' @examples
#' data <- data.frame(
#'   supp = factor(rep(c("OJ", "VC"), each = 3),
#'                 levels = c("OJ", "VC")),
#'   dose = factor(rep(c("low", "medium", "high"), 2),
#'                 levels = c("low", "medium", "high")),
#'   length = c(13.23, 22.7, 24.06, 7.98, 16.77, 26.14),
#'   label = LETTERS[1:6],
#'   stringsAsFactors = FALSE
#' )
#'
#' # example chart 03 -------
#' chart <- ms_linechart(
#'   data = data, x = "dose", y = "length",
#'   group = "supp", labels = "label"
#' )
#' chart <- chart_settings(
#'   x = chart, table = TRUE
#' )
#'
#' chart <- chart_table(chart,
#'   horizontal = TRUE, vertical = FALSE,
#'   outline = TRUE, show_keys = FALSE
#' )
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
