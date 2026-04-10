#' @export
#' @title Add an ms_chart to an Excel sheet
#' @description Add an `ms_chart` object to a sheet in an xlsx workbook
#' created with [officer::read_xlsx()]. The chart data is written into
#' the sheet and the chart is displayed at the specified position.
#' @param x an rxlsx object (created by [officer::read_xlsx()])
#' @param value an `ms_chart` object
#' @param sheet sheet name where the chart and its data will be placed.
#' The sheet must already exist (see [officer::add_sheet()]).
#' @param data_start_col column index where chart data will be written
#' (default 1, i.e. column A)
#' @param data_start_row row index where chart data will be written
#' (default 1)
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
sheet_add_drawing.ms_chart <- function(
  x, value, sheet,
  data_start_col = 1L, data_start_row = 1L,
  from_col = 3L, from_row = 0L,
  to_col = 10L, to_row = 15L,
  ...
) {
  stopifnot(inherits(x, "rxlsx"))

  # write chart data into the sheet
  x <- sheet_write_data(
    x, data = value$data_series, sheet = sheet,
    start_row = data_start_row, start_col = data_start_col
  )

  # generate chart XML for xlsx context
  chart_xml <- format(
    value,
    id_x = "64451712", id_y = "64453248",
    sheetname = sheet,
    drop_ext_data = TRUE
  )

  package_dir <- x$package_dir

  # write chart XML to xl/charts/
  charts_dir <- file.path(package_dir, "xl", "charts")
  dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(charts_dir, "_rels"),
    recursive = TRUE, showWarnings = FALSE
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
    from_col = from_col, from_row = from_row,
    to_col = to_col, to_row = to_row
  )

  invisible(x)
}
