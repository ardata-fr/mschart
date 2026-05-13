#' @title Treemap chart object
#' @description Creation of a treemap chart object that can be inserted
#' in a 'Microsoft' document. Treemap charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Data is hierarchical: one column per level (parent to leaf, left to
#' right) and one numeric column for the leaf values.
#' @param data a data.frame.
#' @param path character vector of column names defining the hierarchy,
#' from outermost (root) to innermost (leaf).
#' @param value column name for the numeric leaf values.
#' @param labels unused for now; reserved for future custom data label
#' columns.
#' @section Per-leaf coloring with `chart_data_fill()`:
#' Per-leaf coloring via [chart_data_fill()] with a named vector works
#' correctly on flat treemaps (`length(path) == 1`). With a hierarchy
#' (`length(path) >= 2`), PowerPoint silently re-maps `<cx:dataPt>`
#' indices in a way that drops `idx="0"` and applies the last specified
#' color to remaining leaves. This is a PowerPoint rendering limitation
#' (also reproducible from Excel-generated chartEx files); the mschart
#' XML output is conformant. For hierarchical treemaps, prefer a single
#' fill color via `chart_data_fill(x, "#HEX")`.
#' @return An `ms_chart` object (subclass `ms_treemapchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()], [chart_theme()]
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
#' tm <- ms_treemapchart(
#'   data = dat, path = c("region", "country", "city"), value = "value"
#' )
#' tm <- chart_labels(tm, title = "Sales by region")
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, tm, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_treemapchart <- function(data, path, value, labels = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(path), length(path) >= 1L)
  stopifnot(is.character(value), length(value) == 1L)

  missing_path <- setdiff(path, names(data))
  if (length(missing_path)) {
    stop(
      "column(s) ",
      paste(shQuote(missing_path), collapse = ", "),
      " not found in data",
      call. = FALSE
    )
  }
  if (!value %in% names(data)) {
    stop("column ", shQuote(value), " not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[value]])) {
    stop("column ", shQuote(value), " must be numeric", call. = FALSE)
  }

  if (
    inherits(data, "data.table") ||
      inherits(data, "tbl_df") ||
      inherits(data, "tbl")
  ) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  # Normalize path columns to character and ensure data_series order is
  # path columns first (one per level, root..leaf) then the value column.
  for (p in path) {
    if (is.factor(data[[p]])) {
      data[[p]] <- as.character(data[[p]])
    } else {
      data[[p]] <- as.character(data[[p]])
    }
  }

  data_series <- data[, c(path, value), drop = FALSE]
  rownames(data_series) <- NULL

  out <- list(
    data = data,
    data_series = data_series,
    path = path,
    value = value,
    # ms_chart compat fields used by setters / print method
    x = path[1],
    y = value,
    xvar = path[1],
    yvar = value,
    group = NULL,
    label_cols = labels,
    asis = FALSE,
    theme = mschart_theme(),
    labels = list(title = NULL, x = NULL, y = NULL),
    layout_id = "treemap"
  )
  class(out) <- c("ms_treemapchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_treemapchart
format.ms_treemapchart <- function(
  x,
  id_x = NULL,
  id_y = NULL,
  sheetname = "sheet1",
  drop_ext_data = FALSE,
  ...
) {
  cx_format_hierarchical(
    x,
    layout_id = "treemap",
    sheetname = sheetname,
    drop_ext_data = drop_ext_data,
    series_layout_pr = "<cx:layoutPr><cx:parentLabelLayout val=\"overlapping\"/></cx:layoutPr>"
  )
}

#' @export
#' @method print ms_treemapchart
print.ms_treemapchart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_treemapchart")))
  cat(sprintf("* path: %s\n", paste(x$path, collapse = " > ")))
  cat(sprintf("* value: %s\n\n", x$value))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
}
