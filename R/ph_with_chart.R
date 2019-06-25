pml_chart <- function(x, value, id_x, id_y){

  charts_dir <- file.path(x$package_dir, "ppt/charts")
  xlsx_dir <- file.path(x$package_dir, "ppt/embeddings")

  dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(charts_dir, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(xlsx_dir, recursive = TRUE, showWarnings = FALSE)

  chart_file <- tempfile(tmpdir = charts_dir, pattern = "chart", fileext = ".xml")
  xlsx_file <- tempfile(tmpdir = xlsx_dir, pattern = "data", fileext = ".xlsx")
  rel_filename <- file.path( charts_dir, "_rels", paste0(basename(chart_file), ".rels") )

  rel_str <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
                    "<Relationships  xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/package\" Target=\"../embeddings/%s\"/></Relationships>")
  rel_str <- sprintf( rel_str, basename(xlsx_file) )
  cat(rel_str, file = rel_filename)

  write_xlsx( x = list( "sheet1" = value$data_series ), path = xlsx_file)
  xml_elt <- format(value, id_x = id_x, id_y = id_y)

  con <- file(chart_file, open = "wt", encoding = "UTF-8")
  sink(con)
  cat(xml_elt)
  sink()
  close(con)

  slide <- x$slide$get_slide(x$cursor)
  next_id <- slide$relationship()$get_next_id()
  slide$relationship()$add(paste0("rId", next_id),
                           type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart",
                           target = paste0("../charts/", basename(chart_file) ) )

  reference_ <- "<c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"rId%.0f\"/>"
  reference_ <- sprintf(reference_, next_id)
  graphic_frame <- paste0(
    "<p:graphicFrame ",
    "xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" ",
    "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" ",
    "xmlns:p=\"http://schemas.openxmlformats.org/presentationml/2006/main\">",
    "<p:nvGraphicFramePr>",
    "<p:cNvPr id=\"\" name=\"\"/>",
    "<p:cNvGraphicFramePr><a:graphicFrameLocks noGrp=\"true\"/></p:cNvGraphicFramePr>",
    "<p:nvPr/>",
    "</p:nvGraphicFramePr>",
    "<p:xfrm rot=\"0\">",
    "<a:off x=\"0\" y=\"0\"/>",
    "<a:ext cx=\"0\" cy=\"0\"/>",
    "</p:xfrm>",
    "<a:graphic>",
    "<a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/chart\">",
    reference_,
    "</a:graphicData>",
    "</a:graphic>",
    "</p:graphicFrame>"
  )

  partname <- file.path( "/ppt/charts", basename(chart_file) )
  override <- setNames("application/vnd.openxmlformats-officedocument.drawingml.chart+xml", partname )
  x$content_type$add_override(value = override)
  graphic_frame
}

#' @export
#' @importFrom stats setNames
#' @importFrom writexl write_xlsx
#' @importFrom xml2 read_xml xml_find_first xml_replace as_xml_document xml_add_child write_xml
#' @title add chart into a PowerPoint slide
#' @description add a chart as a new shape in the current slide.
#' These functions will be deprecated
#' in the next release and function \code{\link{ph_with.ms_chart}} should
#' be used instead.#' @param x an rpptx object
#' @param x a pptx device
#' @param chart \code{ms_chart} object
#' @param type placeholder type
#' @param index placeholder index (integer). This is to be used when a placeholder type
#' is not unique in the current slide, e.g. two placeholders with type 'body'.
#' @importFrom officer ph_from_xml
#' @examples
#' my_barchart <- ms_barchart(data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' my_barchart <- chart_settings( x = my_barchart,
#'   dir="vertical", grouping="clustered", gap_width = 50 )
#' my_barchart <- chart_ax_x( x= my_barchart,
#'   cross_between = 'between', major_tick_mark="out")
#' my_barchart <- chart_ax_y( x= my_barchart,
#'   cross_between = "midCat", major_tick_mark="in")
#'
#' \donttest{
#' library(officer)
#' doc <- read_pptx()
#' doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
#' doc <- ph_with_chart(doc, chart = my_barchart)
#'
#' fileout <- tempfile(fileext = ".pptx")
#' print(doc, target = fileout)
#' }
ph_with_chart <- function( x, chart, type = "body", index = 1 ){
  stopifnot(inherits(x, "rpptx"))
  graphic_frame <- pml_chart(x, chart, id_x = "64451712", id_y = "64453248")
  x$content_type$add_ext(extension = "xlsx", type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  ph_from_xml(x = x, value = graphic_frame, type = type, index = index )
}

#' @export
#' @param left,top location of chart on the slide
#' @param height,width Height and width in inches.
#' @rdname ph_with_chart
#' @importFrom officer ph_from_xml_at
ph_with_chart_at <- function( x, chart, left, top, width, height ){
  stopifnot(inherits(x, "rpptx"))
  graphic_frame <- pml_chart(x, chart, id_x = "64451712", id_y = "64453248")
  x$content_type$add_ext(extension = "xlsx", type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  ph_from_xml_at(x = x, value = graphic_frame, left = left, top = top, width = width, height = height )
}

#' @importFrom officer ph_with
#' @importFrom xml2 as_xml_document
#' @export
#' @title add a MS Chart output into a PowerPoint object
#' @description produces a Microsoft Chart graphics output from R instructions
#' and add the result in a PowerPoint document object produced
#' by \code{\link[officer]{read_pptx}}.
#' @param x a pptx device
#' @param value chart object
#' @param ... Arguments to be passed to methods, argument
#' \code{location} is mandatory.
#' @examples
#' my_barchart <- ms_barchart(data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' my_barchart <- chart_settings( x = my_barchart,
#'   dir="vertical", grouping="clustered", gap_width = 50 )
#' my_barchart <- chart_ax_x( x= my_barchart,
#'   cross_between = 'between', major_tick_mark="out")
#' my_barchart <- chart_ax_y( x= my_barchart,
#'   cross_between = "midCat", major_tick_mark="in")
#'
#' library(officer)
#' doc <- read_pptx()
#' doc <- add_slide(doc, "Title and Content", "Office Theme")
#' doc <- ph_with(doc, my_barchart, location = ph_location_fullsize())
#'
#' fileout <- tempfile(fileext = ".pptx")
#' print(doc, target = fileout)
ph_with.ms_chart <- function( x, value, ... ){
  stopifnot(inherits(x, "rpptx"))
  graphic_frame <- pml_chart(x, value, id_x = "64451712", id_y = "64453248")
  x$content_type$add_ext(extension = "xlsx", type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  ph_with(x = x, value = as_xml_document(graphic_frame), ... )
}
