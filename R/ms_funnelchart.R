#' @title Funnel chart object
#' @description Creation of a funnel chart object that can be inserted
#' in a 'Microsoft' document. Funnel charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Each row is one stage of the funnel. Values are typically decreasing
#' (e.g. visitors -> leads -> customers). Bars are centered horizontally
#' and width is proportional to the value.
#' @param data a data.frame.
#' @param x category column name (stage label).
#' @param y numeric value column name.
#' @return An `ms_chart` object (subclass `ms_funnelchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()]
#' @export
#' @examples
#' library(officer)
#'
#' dat <- data.frame(
#'   stage = c("Visitors", "Leads", "Opportunities", "Quotes", "Customers"),
#'   count = c(5000, 4000, 3000, 1000, 250),
#'   stringsAsFactors = FALSE
#' )
#' fn <- ms_funnelchart(data = dat, x = "stage", y = "count")
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, fn, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_funnelchart <- function(data, x, y) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x), length(x) == 1L)
  stopifnot(is.character(y), length(y) == 1L)

  if (!x %in% names(data)) {
    stop("column ", shQuote(x), " not found in data", call. = FALSE)
  }
  if (!y %in% names(data)) {
    stop("column ", shQuote(y), " not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[y]])) {
    stop("column ", shQuote(y), " must be numeric", call. = FALSE)
  }
  if (
    inherits(data, "data.table") ||
      inherits(data, "tbl_df") ||
      inherits(data, "tbl")
  ) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  data[[x]] <- as.character(data[[x]])

  data_series <- data[, c(x, y), drop = FALSE]
  rownames(data_series) <- NULL

  out <- list(
    data = data,
    data_series = data_series,
    x = x,
    y = y,
    xvar = x,
    yvar = y,
    group = NULL,
    asis = FALSE,
    theme = mschart_theme(),
    labels = list(title = NULL, x = NULL, y = NULL),
    layout_id = "funnel"
  )
  class(out) <- c("ms_funnelchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_funnelchart
format.ms_funnelchart <- function(
  x,
  id_x = NULL,
  id_y = NULL,
  sheetname = "sheet1",
  drop_ext_data = FALSE,
  ...
) {
  ds <- x$data_series
  n <- nrow(ds)
  cat_col <- x$x
  val_col <- x$y
  row1 <- 2L
  row2 <- n + 1L

  cat_dim <- paste0(
    "<cx:strDim type=\"cat\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, "A", row1, row2)),
    cx_str_lvl(ds[[cat_col]]),
    "</cx:strDim>"
  )
  val_dim <- paste0(
    "<cx:numDim type=\"val\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, "B", row1, row2)),
    cx_num_lvl(ds[[val_col]], format_code = "Standard"),
    "</cx:numDim>"
  )

  unique_id <- x$unique_id %||% uuid_generate()
  fill <- cx_render_series_fill(x)
  series_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"funnel\" uniqueId=\"%s\">",
      unique_id
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, "B", 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(val_col)),
    "</cx:txData></cx:tx>",
    fill$spPr,
    fill$dataPts,
    cx_data_labels_xml(
      defaults = list(show_val = TRUE, show_cat = FALSE, show_serie = FALSE),
      opts = x$cx_data_labels,
      fp = x$cx_data_labels$fp
    ),
    "<cx:dataId val=\"0\"/>",
    "</cx:series>"
  )

  axx <- x$cx_axis_x %||% list()
  axes_xml <- cx_axis(
    "1",
    "<cx:catScaling gapWidth=\"0.0599999987\"/>",
    fp = x$theme$axis_text_x,
    title = x$labels[["x"]],
    title_fp = x$theme$axis_title_x,
    num_fmt = axx$num_fmt
  )

  title_xml <- cx_chart_title(x$labels[["title"]], fp = x$theme$main_title)

  ext_data_xml <- if (drop_ext_data) {
    ""
  } else {
    "<cx:externalData r:id=\"rId1\" cx:autoUpdate=\"0\"/>"
  }

  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<cx:chartSpace",
    " xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"",
    " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"",
    " xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\">",
    "<cx:chartData>",
    ext_data_xml,
    "<cx:data id=\"0\">",
    cat_dim,
    val_dim,
    "</cx:data>",
    "</cx:chartData>",
    "<cx:chart>",
    title_xml,
    "<cx:plotArea><cx:plotAreaRegion>",
    series_xml,
    "</cx:plotAreaRegion>",
    axes_xml,
    "</cx:plotArea>",
    "</cx:chart>",
    "</cx:chartSpace>"
  )
}

#' @export
#' @method print ms_funnelchart
print.ms_funnelchart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_funnelchart")))
  cat(sprintf("* x: %s\n", x$x))
  cat(sprintf("* y: %s\n\n", x$y))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
}
