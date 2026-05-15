#' @title Pareto chart object
#' @description Creation of a Pareto chart object that can be inserted
#' in a 'Microsoft' document. Pareto charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Office draws columns sorted by descending count plus a cumulative
#' line on a secondary percentage axis.
#'
#' Two input modes are supported:
#' * `aggregate = TRUE` (default): `data` is in long format (one row per
#'   observation) and Office counts occurrences of each `x` value.
#'   `y` is optional; when supplied, values are summed per category.
#' * `aggregate = FALSE`: `data` is already aggregated (one row per
#'   category) and `y` is the count/value column.
#' @param data a data.frame.
#' @param x category column name.
#' @param y optional numeric column. With `aggregate=TRUE` and `y=NULL`,
#' each row counts as 1.
#' @param aggregate logical, see Description. Default TRUE.
#' @return An `ms_chart` object (subclass `ms_paretochart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()]
#' @export
#' @examples
#' library(officer)
#'
#' set.seed(1)
#' dat <- data.frame(
#'   defect = sample(c("A", "B", "C", "D"), 50, replace = TRUE,
#'                   prob = c(0.5, 0.25, 0.15, 0.1))
#' )
#' pa <- ms_paretochart(dat, x = "defect")
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, pa, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_paretochart <- function(data, x, y = NULL, aggregate = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x), length(x) == 1L)
  if (!x %in% names(data)) {
    stop("column ", shQuote(x), " not found in data", call. = FALSE)
  }
  if (!is.null(y)) {
    stopifnot(is.character(y), length(y) == 1L)
    if (!y %in% names(data)) {
      stop("column ", shQuote(y), " not found in data", call. = FALSE)
    }
    if (!is.numeric(data[[y]])) {
      stop("column ", shQuote(y), " must be numeric", call. = FALSE)
    }
  }
  if (
    inherits(data, "data.table") ||
      inherits(data, "tbl_df") ||
      inherits(data, "tbl")
  ) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  data[[x]] <- as.character(data[[x]])

  # Build the embedded series: when y is omitted, fill column B with 1s
  # (Office aggregates these to a count). When y is provided, keep it.
  # data_series has a fixed positional layout (col 1 = category, col 2 = value);
  # downstream code accesses it by position, so column names only matter as the
  # Excel header. Use make.unique to avoid duplicate headers when the user's
  # x column happens to share its name with the synthesized value column.
  y_label <- y %||% "count"
  if (is.null(y)) {
    data_series <- data.frame(
      data[[x]],
      rep(1L, nrow(data)),
      stringsAsFactors = FALSE
    )
  } else {
    data_series <- data[, c(x, y), drop = FALSE]
  }
  names(data_series) <- make.unique(c(x, y_label))
  rownames(data_series) <- NULL

  out <- list(
    data = data,
    data_series = data_series,
    x = x,
    y = y_label,
    xvar = x,
    yvar = y_label,
    group = NULL,
    asis = FALSE,
    theme = mschart_theme(),
    labels = list(title = NULL, x = NULL, y = NULL),
    layout_id = "paretoLine",
    aggregate = isTRUE(aggregate),
    options = list(line = NULL)
  )
  class(out) <- c("ms_paretochart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_paretochart
format.ms_paretochart <- function(
  x,
  id_x = NULL,
  id_y = NULL,
  sheetname = "sheet1",
  drop_ext_data = FALSE,
  ...
) {
  ds <- x$data_series
  n <- nrow(ds)
  # data_series has a fixed layout: col 1 = category, col 2 = value.
  # Access by position so that user column names (e.g. x named "count") cannot
  # collide with the synthesized value column. x$y stays as the display label.
  row1 <- 2L
  row2 <- n + 1L

  cat_dim <- paste0(
    "<cx:strDim type=\"cat\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, "A", row1, row2)),
    cx_str_lvl(ds[[1L]]),
    "</cx:strDim>"
  )
  val_dim <- paste0(
    "<cx:numDim type=\"val\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, "B", row1, row2)),
    cx_num_lvl(ds[[2L]], format_code = "Standard"),
    "</cx:numDim>"
  )

  unique_id_col <- x$unique_id %||% uuid_generate()
  unique_id_line <- uuid_generate()
  agg_xml <- if (x$aggregate) "<cx:aggregation/>" else ""
  fill <- cx_render_series_fill(x)

  series_col_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"clusteredColumn\" uniqueId=\"%s\">",
      unique_id_col
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, "B", 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(x$y)),
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
    agg_xml,
    "</cx:layoutPr>",
    "<cx:axisId val=\"1\"/>",
    "</cx:series>"
  )

  series_line_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"paretoLine\" ownerIdx=\"0\" uniqueId=\"%s\">",
      unique_id_line
    ),
    cx_default_line_spPr(x$options$line),
    "<cx:axisId val=\"2\"/>",
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
    ),
    # Axis 2 (cumulative percentage) is fixed by the pareto layout.
    cx_axis(
      "2",
      "<cx:valScaling max=\"1\" min=\"0\"/>",
      "<cx:units unit=\"percentage\"/>",
      fp = x$theme$axis_text_y
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
    cat_dim,
    val_dim,
    "</cx:data>",
    "</cx:chartData>",
    "<cx:chart>",
    title_xml,
    "<cx:plotArea><cx:plotAreaRegion>",
    series_col_xml,
    series_line_xml,
    "</cx:plotAreaRegion>",
    axes_xml,
    "</cx:plotArea>",
    "</cx:chart>",
    "</cx:chartSpace>"
  )
}

#' @export
#' @method print ms_paretochart
print.ms_paretochart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_paretochart")))
  cat(sprintf("* x: %s\n", x$x))
  cat(sprintf("* y: %s\n", x$y))
  cat(sprintf("* aggregate: %s\n\n", x$aggregate))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), , drop = FALSE])
}
