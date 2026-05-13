#' @title Histogram chart object
#' @description Creation of a histogram chart object that can be inserted
#' in a 'Microsoft' document. Histogram charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Data is a single column of raw numeric observations. Office computes
#' the bins automatically; pass `bin_count` or `bin_width` to override.
#' @param data a data.frame.
#' @param value numeric column name (raw observations).
#' @param bin_count integer, requested number of bins. Mutually exclusive
#' with `bin_width`. NULL = automatic.
#' @param bin_width numeric, requested bin width. Mutually exclusive with
#' `bin_count`. NULL = automatic.
#' @param interval_closed one of `"right"` (default) or `"left"`. Defines
#' which end of each bin is inclusive.
#' @param underflow numeric, values below this go in an "underflow" bin.
#' NULL to disable.
#' @param overflow numeric, values above this go in an "overflow" bin.
#' NULL to disable.
#' @return An `ms_chart` object (subclass `ms_histogramchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()]
#' @export
#' @examples
#' library(officer)
#'
#' set.seed(1)
#' dat <- data.frame(x = rnorm(200, mean = 10, sd = 4))
#' hi <- ms_histogramchart(dat, value = "x", bin_count = 12)
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, hi, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_histogramchart <- function(
  data,
  value,
  bin_count = NULL,
  bin_width = NULL,
  interval_closed = c("right", "left"),
  underflow = NULL,
  overflow = NULL
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(value), length(value) == 1L)
  interval_closed <- match.arg(interval_closed)

  if (!value %in% names(data)) {
    stop("column ", shQuote(value), " not found in data", call. = FALSE)
  }
  if (!is.numeric(data[[value]])) {
    stop("column ", shQuote(value), " must be numeric", call. = FALSE)
  }
  if (!is.null(bin_count) && !is.null(bin_width)) {
    stop("provide either 'bin_count' or 'bin_width', not both", call. = FALSE)
  }
  if (
    inherits(data, "data.table") ||
      inherits(data, "tbl_df") ||
      inherits(data, "tbl")
  ) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  data_series <- data[, value, drop = FALSE]
  rownames(data_series) <- NULL

  out <- list(
    data = data,
    data_series = data_series,
    value = value,
    x = value,
    y = value,
    xvar = value,
    yvar = value,
    group = NULL,
    asis = FALSE,
    theme = mschart_theme(),
    labels = list(title = NULL, x = NULL, y = NULL),
    layout_id = "clusteredColumn",
    bin_count = bin_count,
    bin_width = bin_width,
    interval_closed = interval_closed,
    underflow = underflow,
    overflow = overflow
  )
  class(out) <- c("ms_histogramchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_histogramchart
format.ms_histogramchart <- function(
  x,
  id_x = NULL,
  id_y = NULL,
  sheetname = "sheet1",
  drop_ext_data = FALSE,
  ...
) {
  ds <- x$data_series
  n <- nrow(ds)
  val_col <- x$value
  row1 <- 2L
  row2 <- n + 1L

  val_dim <- paste0(
    "<cx:numDim type=\"val\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, "A", row1, row2)),
    cx_num_lvl(ds[[val_col]], format_code = "Standard"),
    "</cx:numDim>"
  )

  # Per chartEx schema (CT_Binning): intervalClosed/underflow/overflow are
  # attributes; binCount and binSize are CHILD ELEMENTS with a `val` attr.
  closed_attr <- if (x$interval_closed == "right") "r" else "l"
  binning_attrs <- sprintf("intervalClosed=\"%s\"", closed_attr)
  if (!is.null(x$underflow)) {
    binning_attrs <- paste0(
      binning_attrs,
      sprintf(
        " underflow=\"%s\"",
        format(x$underflow, scientific = FALSE, trim = TRUE)
      )
    )
  }
  if (!is.null(x$overflow)) {
    binning_attrs <- paste0(
      binning_attrs,
      sprintf(
        " overflow=\"%s\"",
        format(x$overflow, scientific = FALSE, trim = TRUE)
      )
    )
  }

  binning_children <- ""
  if (!is.null(x$bin_count)) {
    binning_children <- sprintf(
      "<cx:binCount val=\"%d\"/>",
      as.integer(x$bin_count)
    )
  }
  if (!is.null(x$bin_width)) {
    binning_children <- sprintf(
      "<cx:binSize val=\"%s\"/>",
      format(x$bin_width, scientific = FALSE, trim = TRUE)
    )
  }
  binning_xml <- if (nzchar(binning_children)) {
    sprintf("<cx:binning %s>%s</cx:binning>", binning_attrs, binning_children)
  } else {
    sprintf("<cx:binning %s/>", binning_attrs)
  }

  unique_id <- x$unique_id %||% cx_unique_id()
  fill <- cx_render_series_fill(x)
  series_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"clusteredColumn\" uniqueId=\"%s\">",
      unique_id
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, "A", 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(val_col)),
    "</cx:txData></cx:tx>",
    fill$spPr,
    fill$dataPts,
    cx_data_labels_xml(
      defaults = NULL,
      opts = x$cx_data_labels,
      fp = x$cx_data_labels$fp
    ),
    "<cx:dataId val=\"0\"/>",
    "<cx:layoutPr>",
    binning_xml,
    "</cx:layoutPr>",
    "</cx:series>"
  )

  axx <- x$cx_axis_x %||% list()
  axy <- x$cx_axis_y %||% list()
  axes_xml <- paste0(
    cx_axis(
      "0",
      "<cx:catScaling gapWidth=\"0\"/>",
      fp = x$theme$axis_text_x,
      title = x$labels[["x"]],
      title_fp = x$theme$axis_title_x,
      num_fmt = axx$num_fmt
    ),
    cx_axis(
      "1",
      cx_val_scaling(min = axy$limit_min, max = axy$limit_max),
      fp = x$theme$axis_text_y,
      title = x$labels[["y"]],
      title_fp = x$theme$axis_title_y,
      major_grid = axy$major_grid %||% TRUE,
      minor_grid = axy$minor_grid %||% FALSE,
      num_fmt = axy$num_fmt
    )
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
#' @method print ms_histogramchart
print.ms_histogramchart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_histogramchart")))
  cat(sprintf("* value: %s\n", x$value))
  if (!is.null(x$bin_count)) {
    cat(sprintf("* bin_count: %d\n", x$bin_count))
  }
  if (!is.null(x$bin_width)) {
    cat(sprintf("* bin_width: %s\n", x$bin_width))
  }
  cat(sprintf("* interval closed: %s\n\n", x$interval_closed))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), , drop = FALSE])
}
