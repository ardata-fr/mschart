#' @export
#' @title Modify data labels settings
#' @description Data labels show details about data series. S3 generic;
#' the `default` method is documented below. ChartEx charts honor a
#' leaner set of options.
#' @param x an `ms_chart` object.
#' @param ... arguments passed to S3 methods.
#' @seealso [chart_labels_text()], [chart_labels()]
chart_data_labels <- function(x, ...) UseMethod("chart_data_labels")

#' @rdname chart_data_labels
#' @param num_fmt `character(1)`: number formatting specifies number format properties which
#' indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0",
#' "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param position `character(1)`: it specifies the position of the data label.
#' It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l',
#' 'outEnd', 'r', 't'.
#' When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'.
#' When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'.
#' When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param show_legend_key show legend key if TRUE.
#' @param show_val show values if TRUE.
#' @param show_cat_name show categories if TRUE.
#' @param show_serie_name show names of series if TRUE.
#' @param show_percent show percentages if TRUE.
#' @param separator separator between the label components (value, category
#' name, series name, etc.) when multiple components are displayed.
#' Default is `", "`.
#' @return An `ms_chart` object.
#' @export
#' @method chart_data_labels default
#' @examples
#' my_bc <- ms_barchart(
#'   data = browser_data, x = "browser",
#'   y = "value", group = "serie"
#' )
#' my_bc <- chart_data_labels(my_bc, show_val = TRUE, position = "outEnd")
chart_data_labels.default <- function(
  x,
  num_fmt = "General",
  position = "ctr",
  show_legend_key = FALSE,
  show_val = FALSE,
  show_cat_name = FALSE,
  show_serie_name = FALSE,
  show_percent = FALSE,
  separator = ", ",
  ...
) {
  if (!position %in% st_dlblpos) {
    stop(
      "position should be one of ",
      paste0(shQuote(st_dlblpos), collapse = ", ")
    )
  }

  out <- list(
    num_fmt = num_fmt,
    position = position,
    show_legend_key = show_legend_key,
    show_val = show_val,
    show_cat_name = show_cat_name,
    show_serie_name = show_serie_name,
    show_percent = show_percent,
    separator = separator
  )
  class(out) <- "labels_options"
  x$label_settings <- out
  x
}

#' @export
#' @method to_pml labels_options
to_pml.labels_options <- function(
  x,
  add_ns = FALSE,
  with_position = TRUE,
  show_label = FALSE,
  ...
) {
  txpr <- ""
  if (!is.null(x$labels_fp)) {
    txpr <- ooxml_txpr(x$labels_fp)
  }

  str_ <- paste0(
    "<c:dLbls>",
    sprintf(
      "<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>",
      htmlEscape(x$num_fmt, attribute = TRUE)
    ),
    txpr,
    if (with_position) sprintf("<c:dLblPos val=\"%s\"/>", x$position),
    sprintf("<c:showLegendKey val=\"%.0f\"/>", x$show_legend_key),
    sprintf("<c:showVal val=\"%.0f\"/>", as.integer(x$show_val)),
    sprintf("<c:showCatName val=\"%.0f\"/>", as.integer(x$show_cat_name)),
    sprintf("<c:showSerName val=\"%.0f\"/>", as.integer(x$show_serie_name)),
    sprintf("<c:showPercent val=\"%.0f\"/>", x$show_percent),
    sprintf("<c:showBubbleSize val=\"%.0f\"/>", FALSE),
    sprintf("<c:separator val=\"%s\"/>", x$separator),
    if (show_label) {
      paste0(
        "<c:extLst>",
        "<c:ext uri=\"{CE6537A1-D6FC-4f65-9D91-7224C49458BB}\" xmlns:c15=\"http://schemas.microsoft.com/office/drawing/2012/chart\">",
        "<c15:dlblFieldTable/>",
        "<c15:showDataLabelsRange val=\"1\"/>",
        "</c:ext>",
        "</c:extLst>"
      )
    },
    "</c:dLbls>"
  )

  str_
}

#' @export
#' @method chart_data_labels ms_chart_ex
chart_data_labels.ms_chart_ex <- function(
  x,
  show_val = NULL,
  show_cat = NULL,
  show_serie = NULL,
  position = NULL,
  num_fmt = NULL,
  ...
) {
  cur <- x$cx_data_labels %||% list()
  if (!is.null(show_val)) {
    cur$show_val <- isTRUE(show_val)
  }
  if (!is.null(show_cat)) {
    cur$show_cat <- isTRUE(show_cat)
  }
  if (!is.null(show_serie)) {
    cur$show_serie <- isTRUE(show_serie)
  }
  if (!is.null(position)) {
    cur$position <- position
  }
  if (!is.null(num_fmt)) {
    cur$num_fmt <- num_fmt
  }
  x$cx_data_labels <- cur
  x
}
