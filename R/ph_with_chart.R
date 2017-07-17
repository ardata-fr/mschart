#' @export
#' @importFrom openxlsx write.xlsx
#' @importFrom xml2 read_xml xml_find_first xml_replace as_xml_document xml_add_child write_xml
ph_with_chart <- function( x, value, type = "body", index = 1 ){

  stopifnot( type %in% c("ctrTitle", "subTitle", "dt", "ftr", "sldNum", "title", "body") )

  slide <- x$slide$get_slide(x$cursor)
  xfrm_df <- slide$get_xfrm(type = type, index = index)
  left = xfrm_df$offx
  top = xfrm_df$offy
  width = xfrm_df$cx
  height = xfrm_df$cy


  # filenames and dirs ----
  charts_dir <- file.path(x$package_dir, "ppt/charts")
  xlsx_dir <- file.path(x$package_dir, "ppt/embeddings")
  dir.create(charts_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(charts_dir, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(xlsx_dir, recursive = TRUE, showWarnings = FALSE)
  chart_file <- tempfile(tmpdir = charts_dir, pattern = "chart", fileext = ".xml")
  xlsx_file <- tempfile(tmpdir = xlsx_dir, pattern = "data", fileext = ".xlsx")
  rel_filename <- file.path( charts_dir, "_rels", paste0(basename(chart_file), ".rels") )


  xml_template <- system.file(package = "officer", "template", "chart.xml")
  xml_doc <- read_xml(xml_template)


  rel_str <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
  "<Relationships  xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/package\" Target=\"../embeddings/%s\"/></Relationships>")
  rel_str <- sprintf( rel_str, basename(xlsx_file) )
  cat(rel_str, file = rel_filename)

  write.xlsx(value$get_data(), file = xlsx_file, sheetName = "sheet1")

  xml_elt <- value$pml()

  node <- xml_find_first(xml_doc, "//c:plotArea")
  xml_replace( node, as_xml_document(xml_elt) )

  write_xml(xml_doc, chart_file)


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
    sprintf( "<a:off x=\"%.0f\" y=\"%.0f\"/>", left*914400, top*914400 ),
    sprintf( "<a:ext cx=\"%.0f\" cy=\"%.0f\"/>", width*914400, height*914400 ),
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

  xml_add_child(xml_find_first(slide$get(), "//p:spTree"), as_xml_document(graphic_frame))
  slide$save()
  x$slide$update()
  x
}


