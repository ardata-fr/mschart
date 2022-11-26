#' @title xml code for table in chart
#' @param x an object of class table_options
#' @noRd
table_content_xml <- function(x) {
  x_horizontal_id <- ifelse(x$x_table$horizontal, 1, 0)
  x_vertical_id <- ifelse(x$x_table$vertical, 1, 0)
  x_outline_id <- ifelse(x$x_table$outline, 1, 0)
  x_show_keys_id <- ifelse(x$x_table$show_keys, 1, 0)

  txpr <- ""
  if (!is.null(x$labels_fp)) {
    txpr <- ooxml_txpr(x$labels_fp)
  } else {
    txpr <- ooxml_txpr(x$theme$table_text)
  }

  if (x$options$table) {
    table_str <- paste0(
      "<c:dTable>",
      sprintf("<c:showHorzBorder val=\"%s\"/>", x_horizontal_id),
      sprintf("<c:showVertBorder val=\"%s\"/>", x_vertical_id),
      sprintf("<c:showOutline val=\"%s\"/>", x_outline_id),
      sprintf("<c:showKeys val=\"%s\"/>", x_show_keys_id),
      txpr,
      "</c:dTable>"
    )
  } else {
    table_str <- NULL
  }
  table_str
}
