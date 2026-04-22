#' @export
#' @title Add an ms_chart to an Excel sheet
#' @description Add an `ms_chart` object to a sheet in an xlsx workbook
#' created with [officer::read_xlsx()]. The chart data is written into
#' the sheet and the chart is displayed at the specified position.
#' @param x an rxlsx object (created by [officer::read_xlsx()])
#' @param value an `ms_chart` object
#' @param sheet sheet name where the chart and its data will be placed.
#' The sheet must already exist (see [officer::add_sheet()]).
#' @param start_col column index where chart data will be written
#' (default 1, i.e. column A). When `write_data = FALSE`, this is still
#' the cell position that the chart XML will point at, but no data is
#' written.
#' @param start_row row index where chart data will be written
#' (default 1). Same semantics with `write_data = FALSE` as for
#' `start_col`.
#' @param write_data if `TRUE` (the default), the chart's
#' `data_series` is written into the sheet at
#' `(start_col, start_row)` before the chart is added.
#' Pass `FALSE` when the data is already present in the sheet (for
#' example to avoid rewriting it when several charts share the same
#' dataset, or when inserting a chart that references data written
#' independently via [officer::sheet_write_data()]).
#' @param from_col,from_row top-left anchor of the chart (0-based)
#' @param to_col,to_row bottom-right anchor of the chart (0-based)
#' @param ... unused
#' @return the rxlsx object (invisibly)
#' @importFrom officer sheet_write_data sheet_add_drawing xlsx_drawing
#' @importFrom xml2 read_xml xml_find_all xml_attr as_xml_document
#' @method sheet_add_drawing ms_chart
#' @examples
#' library(officer)
#' library(mschart)
#'
#' my_chart <- ms_barchart(
#'   data = data.frame(
#'     x = c("A", "B", "C"),
#'     y = c(1, 3, 2),
#'     group = rep("serie1", 3)
#'   ),
#'   x = "x", y = "y", group = "group"
#' )
#'
#' x <- read_xlsx()
#' x <- add_sheet(x, label = "chart_sheet")
#' x <- sheet_add_drawing(x, value = my_chart, sheet = "chart_sheet")
#' print(x, target = tempfile(fileext = ".xlsx"))
#'
#' # Sharing one dataset between several charts on the same sheet:
#' # write the data once, then add each chart with write_data = FALSE.
#' shared <- data.frame(
#'   x = c("A", "B", "C"),
#'   y = c(1, 3, 2),
#'   group = rep("serie1", 3)
#' )
#' chart_a <- ms_barchart(shared, x = "x", y = "y", group = "group")
#' chart_b <- ms_linechart(shared, x = "x", y = "y", group = "group")
#'
#' x <- read_xlsx()
#' x <- add_sheet(x, label = "multi")
#' x <- sheet_write_data(x, value = chart_a$data_series, sheet = "multi")
#' x <- sheet_add_drawing(x, value = chart_a, sheet = "multi",
#'                        write_data = FALSE,
#'                        from_col = 4L,  from_row = 0L,
#'                        to_col   = 11L, to_row   = 15L)
#' x <- sheet_add_drawing(x, value = chart_b, sheet = "multi",
#'                        write_data = FALSE,
#'                        from_col = 13L, from_row = 0L,
#'                        to_col   = 20L, to_row   = 15L)
#' print(x, target = tempfile(fileext = ".xlsx"))
sheet_add_drawing.ms_chart <- function(
  x,
  value,
  sheet,
  start_col = 1L,
  start_row = 1L,
  write_data = TRUE,
  from_col = 3L,
  from_row = 0L,
  to_col = 10L,
  to_row = 15L,
  ...
) {
  stopifnot(inherits(x, "rxlsx"))

  # write chart data into the sheet (can be skipped when the data
  # has already been put in place by the caller)
  if (isTRUE(write_data)) {
    x <- sheet_write_data(
      x,
      value = value$data_series,
      sheet = sheet,
      start_row = start_row,
      start_col = start_col
    )
  }

  # as_series() reads these offsets to shift <c:cat>/<c:val>/<c:xVal>/
  # <c:yVal>/<c:bubbleSize> ranges accordingly
  value$start_col <- as.integer(start_col)
  value$start_row <- as.integer(start_row)

  # generate chart XML for xlsx context
  chart_xml <- format(
    value,
    id_x = "64451712",
    id_y = "64453248",
    sheetname = sheet,
    drop_ext_data = TRUE
  )

  package_dir <- x$package_dir

  # write chart XML to xl/charts/
  charts_dir <- file.path(package_dir, "xl", "charts")
  dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(charts_dir, "_rels"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  existing_charts <- list.files(charts_dir, pattern = "^chart[0-9]+\\.xml$")
  if (length(existing_charts) == 0L) {
    chart_name <- "chart1.xml"
  } else {
    nums <- as.integer(gsub("\\D", "", existing_charts))
    chart_name <- sprintf("chart%d.xml", max(nums) + 1L)
  }

  chart_file <- file.path(charts_dir, chart_name)
  writeLines(chart_xml, chart_file, useBytes = TRUE)

  # empty rels for the chart
  chart_rels <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"/>"
  )
  writeLines(
    chart_rels,
    file.path(charts_dir, "_rels", paste0(chart_name, ".rels")),
    useBytes = TRUE
  )

  # content type
  partname <- paste0("/xl/charts/", chart_name)
  x$content_type$add_override(setNames(
    "application/vnd.openxmlformats-officedocument.drawingml.chart+xml",
    partname
  ))

  # get or create drawing for the sheet
  sheet_idx <- which(x$worksheets$sheet_names() == sheet)
  sheet_obj <- x$sheets$get_sheet(sheet_idx)
  drw <- xlsx_drawing$new(package_dir, sheet_obj, x$content_type)

  # add chart relationship and anchor in drawing
  chart_rid <- drw$add_chart_rel(chart_name)
  drw$add_chart_anchor(
    chart_rid,
    from_col = from_col,
    from_row = from_row,
    to_col = to_col,
    to_row = to_row
  )

  invisible(x)
}
