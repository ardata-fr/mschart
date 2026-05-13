#' @title Waterfall chart object
#' @description Creation of a waterfall chart object that can be inserted
#' in a 'Microsoft' document. Waterfall charts use the chartEx pipeline
#' (Office 2016+); older versions of 'Microsoft Office' will display a
#' fallback placeholder.
#'
#' Each row is one bar. Positive values rise, negative values fall.
#' Categories listed in `subtotals` are rendered as absolute totals
#' (typical for "Start", intermediate totals, and "End" bars).
#' @param data a data.frame.
#' @param x category column name.
#' @param y numeric value column name. Signed values: positive = up,
#' negative = down. For subtotal rows, the value should be the running
#' total at that point.
#' @param subtotals integer vector of 1-based row indices that should
#' be rendered as subtotal/total bars. Defaults to none.
#' @return An `ms_chart` object (subclass `ms_waterfallchart`).
#' @family 'Office' chart objects
#' @seealso [chart_labels()]
#' @export
#' @examples
#' library(officer)
#'
#' dat <- data.frame(
#'   step = c("Start", "Q1", "Q2", "Q3", "End"),
#'   amount = c(100, 30, -20, 40, 150),
#'   stringsAsFactors = FALSE
#' )
#' wf <- ms_waterfallchart(
#'   data = dat, x = "step", y = "amount",
#'   subtotals = c(1, 5)
#' )
#'
#' doc <- read_pptx()
#' doc <- add_slide(doc)
#' doc <- ph_with(doc, wf, location = ph_location_fullsize())
#' print(doc, target = tempfile(fileext = ".pptx"))
ms_waterfallchart <- function(data, x, y, subtotals = NULL) {
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
  if (is.factor(data[[x]])) {
    data[[x]] <- as.character(data[[x]])
  }
  data[[x]] <- as.character(data[[x]])

  if (!is.null(subtotals)) {
    if (!is.numeric(subtotals)) {
      stop("'subtotals' must be a numeric vector of row indices", call. = FALSE)
    }
    if (any(subtotals < 1L | subtotals > nrow(data))) {
      stop(
        "'subtotals' indices must be between 1 and ",
        nrow(data),
        call. = FALSE
      )
    }
    subtotals <- as.integer(subtotals)
  }

  data_series <- data[, c(x, y), drop = FALSE]
  rownames(data_series) <- NULL

  out <- list(
    data = data,
    data_series = data_series,
    x = x,
    y = y,
    subtotals = subtotals,
    xvar = x,
    yvar = y,
    group = NULL,
    asis = FALSE,
    theme = mschart_theme(),
    labels = list(title = NULL, x = NULL, y = NULL),
    layout_id = "waterfall"
  )
  class(out) <- c("ms_waterfallchart", "ms_chart_ex", "ms_chart")
  out
}

#' @export
#' @method format ms_waterfallchart
format.ms_waterfallchart <- function(
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

  subtotals_xml <- if (length(x$subtotals)) {
    paste0(
      "<cx:layoutPr><cx:subtotals>",
      paste0(
        sprintf("<cx:idx val=\"%d\"/>", x$subtotals - 1L),
        collapse = ""
      ),
      "</cx:subtotals></cx:layoutPr>"
    )
  } else {
    ""
  }

  unique_id <- x$unique_id %||% cx_unique_id()
  fill <- cx_render_series_fill(x)
  series_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"waterfall\" uniqueId=\"%s\">",
      unique_id
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, "B", 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(val_col)),
    "</cx:txData></cx:tx>",
    fill$spPr,
    fill$dataPts,
    cx_data_labels_xml(
      defaults = list(
        position = "outEnd",
        show_val = TRUE,
        show_cat = FALSE,
        show_serie = FALSE
      ),
      opts = x$cx_data_labels,
      fp = x$cx_data_labels$fp
    ),
    "<cx:dataId val=\"0\"/>",
    subtotals_xml,
    "</cx:series>"
  )

  axx <- x$cx_axis_x %||% list()
  axy <- x$cx_axis_y %||% list()
  axes_xml <- paste0(
    cx_axis(
      "0",
      "<cx:catScaling gapWidth=\"0.5\"/>",
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
#' @method print ms_waterfallchart
print.ms_waterfallchart <- function(x, preview = FALSE, ...) {
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
  cat(sprintf("* %s object (chartEx)\n\n", shQuote("ms_waterfallchart")))
  cat(sprintf("* x: %s\n", x$x))
  cat(sprintf("* y: %s\n", x$y))
  if (length(x$subtotals)) {
    cat(sprintf(
      "* subtotals (rows): %s\n",
      paste(x$subtotals, collapse = ", ")
    ))
  }
  cat(sprintf(
    "\n* original data [%.0f,%.0f] (sample):\n",
    nrow(x$data),
    ncol(x$data)
  ))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
}
