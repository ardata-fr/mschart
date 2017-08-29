#' @export
#' @title add chart into a Word document
#' @description add a \code{ms_chart} into an rdocx object, the graphic will be
#' inserted in an empty paragraph.
#' @param x an rdocx object
#' @param chart an \code{ms_chart} object.
#' @param style paragraph style
#' @param pos where to add the new element relative to the cursor,
#' one of "after", "before", "on".
#' @param height,width height and width in inches.
#' @importFrom officer body_add_xml
#' @examples
#' library(officer)
#' my_barchart <- ms_barchart(data = browser_data,
#'   x = "browser", y = "value", group = "serie")
#' my_barchart <- chart_settings( my_barchart, grouping = "stacked",
#'   gap_width = 50, overlap = 100 )
#'
#' doc <- read_docx()
#' doc <- body_add_chart(doc, chart = my_barchart, style = "centered")
#' print(doc, target = "barchart_example.docx")
body_add_chart <- function( x, chart, style = NULL, pos = "after",
                            width = 5, height = 3 ){

  charts_dir <- file.path(x$package_dir, "word/charts")
  xlsx_dir <- file.path(x$package_dir, "word/embeddings")

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
  id_x = "64451712"
  id_y = "64453248"
  write.xlsx(chart$data_series, file = xlsx_file, sheetName = "sheet1")
  xml_elt <- format(chart, id_x = id_x, id_y = id_y)
  cat(xml_elt, file = chart_file)

  next_id <- x$doc_obj$relationship()$get_next_id()
  x$doc_obj$relationship()$add(paste0("rId", next_id),
                           type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart",
                           target = paste0("charts/", basename(chart_file) ) )
  reference_ <- "<c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" r:id=\"rId%.0f\"/>"
  reference_ <- sprintf(reference_, next_id)



  drawing_str <- paste0("<w:drawing><wp:inline distT=\"0\" distB=\"0\" distL=\"0\" distR=\"0\">",
                        sprintf("<wp:extent cx=\"%.0f\" cy=\"%.0f\"/>", width*914400, height*914400),
                        "<wp:effectExtent l=\"0\" t=\"0\" r=\"0\" b=\"0\"/>",
                        "<wp:docPr id=\"\" name=\"\"/>",
                        "<wp:cNvGraphicFramePr/>",
                        "<a:graphic xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">",
                        "<a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/chart\">",
                        reference_, "</a:graphicData>",
                        "</a:graphic>",
                        "</wp:inline>",
                        "</w:drawing>")

  base_ns <- "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\""
  if( is.null(style) )
    style <- x$default_styles$paragraph
  style_id <- x$doc_obj$get_style_id(style=style, type = "paragraph")
  par_elt <- paste0(sprintf("<%s %s>", "w:p", base_ns),
                    "<w:pPr><w:pStyle w:val=\"", style_id, "\"/></w:pPr><w:r>",
                    drawing_str, "</w:r></w:p>")

  partname <- file.path( "/word/charts", basename(chart_file) )
  override <- setNames("application/vnd.openxmlformats-officedocument.drawingml.chart+xml", partname )
  x$content_type$add_override(value = override)

  body_add_xml(x, str = par_elt, pos = pos)
}

