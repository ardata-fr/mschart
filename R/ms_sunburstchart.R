#' @title Sunburst chart object
#' @description Creation of a sunburst chart object that can be inserted
#' in a 'Microsoft' document. Sunburst charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Data is hierarchical: one column per level (parent to leaf, left to
#' right) and one numeric column for the leaf values. Leaf values
#' propagate to parent rings as sums.
#' @param data a data.frame.
#' @param path character vector of column names defining the hierarchy,
#' from outermost (root) to innermost (leaf).
#' @param value column name for the numeric leaf values.
#' @param labels unused for now; reserved for future custom data label
#' columns.
#' @section Per-leaf coloring:
#' Same caveat as [ms_treemapchart()]: per-leaf colors via
#' [chart_data_fill()] only work reliably with `length(path) == 1`.
#' @return An `ms_chart` object (subclass `ms_sunburstchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()], [ms_treemapchart()]
#' @export
#' @examples
#' library(officer)
#'
#' dat <- data.frame(
#'   region  = c("EU", "EU", "EU", "AM", "AM"),
#'   country = c("FR", "FR", "DE", "US", "US"),
#'   city    = c("Paris", "Lyon", "Berlin", "NYC", "LA"),
#'   value   = c(10, 5, 12, 20, 8),
#'   stringsAsFactors = FALSE
#' )
#' sb <- ms_sunburstchart(
#'   data = dat, path = c("region", "country", "city"), value = "value"
#' )
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, sb, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_sunburstchart <- function(data, path, value, labels = NULL) {
  out <- ms_treemapchart(
    data = data,
    path = path,
    value = value,
    labels = labels
  )
  class(out) <- c("ms_sunburstchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_sunburstchart
format.ms_sunburstchart <- function(
  x,
  id_x = NULL,
  id_y = NULL,
  sheetname = "sheet1",
  drop_ext_data = FALSE,
  ...
) {
  cx_format_hierarchical(
    x,
    layout_id = "sunburst",
    sheetname = sheetname,
    drop_ext_data = drop_ext_data,
    series_layout_pr = ""
  )
}

#' @export
#' @method print ms_sunburstchart
print.ms_sunburstchart <- function(x, preview = FALSE, ...) {
  if (preview && interactive()) {
    doc <- officer::read_pptx()
    doc <- officer::add_slide(
      doc,
      layout = "Title and Content",
      master = "Office Theme"
    )
    doc <- officer::ph_with(doc, x, location = officer::ph_location_fullsize())
    file_out <- print(doc, target = tempfile(fileext = ".pptx"))
    utils::browseURL(file_out)
    return(invisible())
  }
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_sunburstchart")))
  cat(sprintf("* path: %s\n", paste(x$path, collapse = " > ")))
  cat(sprintf("* value: %s\n\n", x$value))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
}
