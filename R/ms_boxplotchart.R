#' @title Boxplot chart object
#' @description Creation of a box-and-whisker chart object that can be
#' inserted in a 'Microsoft' document. Boxplot charts use the chartEx
#' pipeline (Office 2016+); older versions of 'Microsoft Office' will
#' display a fallback placeholder.
#'
#' Data is in long format: one row per observation. Office computes
#' quartiles and whiskers from the raw values; do not pre-aggregate.
#' @param data a data.frame.
#' @param x category column name. Each unique value becomes one box.
#' @param y numeric value column name (raw observations).
#' @param quartile_method one of `"exclusive"` (default) or `"inclusive"`.
#' Affects how Q1/Q3 are computed when the count is even.
#' @param show_mean_marker logical, draw the mean as a marker. Default TRUE.
#' @param show_mean_line logical, draw a line connecting means across
#' boxes. Default FALSE.
#' @param show_outliers logical, plot outlier points. Default TRUE.
#' @param show_inner_points logical, plot all non-outlier points. Default FALSE.
#' @return An `ms_chart` object (subclass `ms_boxplotchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()]
#' @export
#' @examples
#' library(officer)
#'
#' set.seed(1)
#' dat <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 20),
#'   value = c(rnorm(20, 0, 5), rnorm(20, 3, 7), rnorm(20, -2, 4))
#' )
#' bp <- ms_boxplotchart(dat, x = "group", y = "value")
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, bp, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_boxplotchart <- function(
  data,
  x,
  y,
  quartile_method = c("exclusive", "inclusive"),
  show_mean_marker = TRUE,
  show_mean_line = FALSE,
  show_outliers = TRUE,
  show_inner_points = FALSE
) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(x), length(x) == 1L)
  stopifnot(is.character(y), length(y) == 1L)
  quartile_method <- match.arg(quartile_method)

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
    layout_id = "boxWhisker",
    quartile_method = quartile_method,
    show_mean_marker = isTRUE(show_mean_marker),
    show_mean_line = isTRUE(show_mean_line),
    show_outliers = isTRUE(show_outliers),
    show_inner_points = isTRUE(show_inner_points),
    options = list(line = NULL)
  )
  class(out) <- c("ms_boxplotchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_boxplotchart
format.ms_boxplotchart <- function(
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

  unique_id <- x$unique_id %||% cx_unique_id()
  fill <- cx_render_series_fill(x)
  # Inject a default stroke override only when chart_data_fill/stroke
  # produced no <cx:spPr>: a series can only carry one <cx:spPr> block
  # so a user override always wins.
  default_line_spPr <- if (nzchar(fill$spPr)) {
    ""
  } else {
    cx_default_line_spPr(x$options$line)
  }
  visibility_attrs <- sprintf(
    "meanLine=\"%d\" meanMarker=\"%d\" nonoutliers=\"%d\" outliers=\"%d\"",
    as.integer(x$show_mean_line),
    as.integer(x$show_mean_marker),
    as.integer(x$show_inner_points),
    as.integer(x$show_outliers)
  )

  series_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"boxWhisker\" uniqueId=\"%s\">",
      unique_id
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, "B", 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(val_col)),
    "</cx:txData></cx:tx>",
    fill$spPr,
    default_line_spPr,
    fill$dataPts,
    cx_data_labels_xml(
      defaults = NULL,
      opts = x$cx_data_labels,
      fp = x$cx_data_labels$fp
    ),
    "<cx:dataId val=\"0\"/>",
    "<cx:layoutPr>",
    sprintf("<cx:visibility %s/>", visibility_attrs),
    sprintf(
      "<cx:statistics quartileMethod=\"%s\"/>",
      x$quartile_method
    ),
    "</cx:layoutPr>",
    "</cx:series>"
  )

  axx <- x$cx_axis_x %||% list()
  axy <- x$cx_axis_y %||% list()
  axes_xml <- paste0(
    cx_axis(
      "0",
      "<cx:catScaling gapWidth=\"1\"/>",
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
#' @method print ms_boxplotchart
print.ms_boxplotchart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_boxplotchart")))
  cat(sprintf("* x: %s\n", x$x))
  cat(sprintf("* y: %s\n", x$y))
  cat(sprintf("* quartile method: %s\n\n", x$quartile_method))
  cat(sprintf(
    "* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
}
