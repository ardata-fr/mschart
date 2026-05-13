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
#' @param left,top top-left anchor of the chart, in inches.
#'   Defaults to `(1, 1)`. Used when `anchor = NULL`. Same convention
#'   as [officer::sheet_add_drawing()] and `rvg::sheet_add_drawing.dml()`.
#' @param width,height size of the chart, in inches. Defaults
#'   to `6 x 4`. Used when `anchor = NULL` or when `anchor` is a single
#'   cell reference.
#' @param anchor optional Excel cell-based anchor. Either `NULL` (the
#'   default; absolute placement via `left`/`top`/`width`/`height`), a
#'   single cell reference like `"B2"` (the chart is anchored to that
#'   cell and keeps `width`/`height`, i.e. "Move but don't size with
#'   cells"), or a range like `"B2:H20"` (the chart is anchored from
#'   the first cell to the second, i.e. Excel's default "Move and size
#'   with cells").
#' @param edit_as one of `"twoCell"`, `"oneCell"`, `"absolute"`. Sets
#'   the `editAs` attribute on `<xdr:twoCellAnchor>`. Ignored unless
#'   `anchor` is a range.
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
#'                        left = 3, top = 0.5, width = 5, height = 3.5)
#' x <- sheet_add_drawing(x, value = chart_b, sheet = "multi",
#'                        write_data = FALSE,
#'                        left = 9, top = 0.5, width = 5, height = 3.5)
#' print(x, target = tempfile(fileext = ".xlsx"))
sheet_add_drawing.ms_chart <- function(
  x,
  value,
  sheet,
  start_col = 1L,
  start_row = 1L,
  write_data = TRUE,
  left = 1,
  top = 1,
  width = 6,
  height = 4,
  anchor = NULL,
  edit_as = c("twoCell", "oneCell", "absolute"),
  ...
) {
  edit_as <- match.arg(edit_as)
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
  # <c:yVal>/<c:bubbleSize> ranges accordingly (classic c: only -- chartEx
  # layouts pin their refs to A1 by construction)
  value$start_col <- as.integer(start_col)
  value$start_row <- as.integer(start_row)

  # generate chart XML for xlsx context (chart references the sheet
  # directly, no embedded xlsx)
  chart_xml <- format(
    value,
    id_x = "64451712",
    id_y = "64453248",
    sheetname = sheet,
    drop_ext_data = TRUE
  )

  package_dir <- x$package_dir

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

  # chartEx requires sidecar style+colors parts, classic charts don't
  extra_rels <- chart_extra_parts(
    value,
    charts_dir,
    x$content_type,
    part_root = "/xl/charts"
  )
  chart_rels <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
    extra_rels,
    "</Relationships>"
  )
  writeLines(
    chart_rels,
    file.path(charts_dir, "_rels", paste0(chart_name, ".rels")),
    useBytes = TRUE
  )

  # content type (c: vs cx: dispatch)
  partname <- paste0("/xl/charts/", chart_name)
  x$content_type$add_override(setNames(
    chart_part_content_type(value),
    partname
  ))

  # get or create drawing for the sheet
  sheet_idx <- which(x$worksheets$sheet_names() == sheet)
  sheet_obj <- x$sheets$get_sheet(sheet_idx)
  drw <- xlsx_drawing$new(package_dir, sheet_obj, x$content_type)

  # drawing -> chart relationship + anchor (officer derives the inner
  # <c:chart>/<cx:chart> fragment from graphic_uri)
  chart_rid <- drw$add_chart_rel(
    chart_name,
    rel_type = chart_part_rel_type(value)
  )
  anchor_args <- list(
    chart_rid,
    graphic_uri = chart_graphicdata_uri(value)
  )
  if (is.null(anchor)) {
    anchor_args <- c(
      anchor_args,
      list(
        left = left,
        top = top,
        width = width,
        height = height
      )
    )
  } else if (!is.character(anchor) || length(anchor) != 1L) {
    stop(
      "`anchor` must be a single string like \"B2\" or \"B2:H20\"",
      call. = FALSE
    )
  } else if (grepl(":", anchor, fixed = TRUE)) {
    parts <- strsplit(anchor, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L || !all(nzchar(parts))) {
      stop(
        "`anchor` range must be \"FROM:TO\" (e.g. \"B2:H20\")",
        call. = FALSE
      )
    }
    anchor_args <- c(
      anchor_args,
      list(
        from = parts[1],
        to = parts[2],
        edit_as = edit_as
      )
    )
  } else {
    anchor_args <- c(
      anchor_args,
      list(
        from = anchor,
        width = width,
        height = height
      )
    )
  }
  do.call(drw$add_chart_anchor, anchor_args)

  invisible(x)
}
