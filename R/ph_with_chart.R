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

  write.xlsx(value$data_series, file = xlsx_file, sheetName = "sheet1")
  xml_elt <- format(value, id_x = id_x, id_y = id_y)
  cat(xml_elt, file = chart_file)

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
#' @importFrom openxlsx write.xlsx
#' @importFrom xml2 read_xml xml_find_first xml_replace as_xml_document xml_add_child write_xml
#' @title add chart into a PowerPoint slide
#' @description add a chart as a new shape in the current slide.
#' @param x a pptx device
#' @param value \code{ms_chart} object
#' @param type placeholder type
#' @param index placeholder index (integer). This is to be used when a placeholder type
#' is not unique in the current slide, e.g. two placeholders with type 'body'.
#' @importFrom officer ph_from_xml
ph_with_chart <- function( x, value, type = "body", index = 1 ){
  stopifnot(inherits(x, "rpptx"))
  graphic_frame <- pml_chart(x, value, id_x = "64451712", id_y = "64453248")
  ph_from_xml(x = x, value = graphic_frame, type = type, index = index )
}

#' @export
#' @param left,top location of chart on the slide
#' @param height,width Height and width in inches.
#' @rdname ph_with_chart
#' @importFrom officer ph_from_xml_at
ph_with_chart_at <- function( x, value, left, top, width, height ){
  stopifnot(inherits(x, "rpptx"))
  graphic_frame <- pml_chart(x, value, id_x = "64451712", id_y = "64453248")
  ph_from_xml_at(x = x, value = graphic_frame, left = left, top = top, width = width, height = height )
}

