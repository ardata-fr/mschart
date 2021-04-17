unpack_chart <- function(chart){

  pptx_out <- tempfile(fileext = ".pptx")
  dir_out <- tempfile(pattern = "dir", fileext = "")

  doc <- read_pptx()
  doc <- add_slide(doc)
  doc <- ph_with(doc, chart, location = ph_location_type())
  print(doc, pptx_out)
  unpack_folder(file = pptx_out, folder = dir_out)

  chart_xml <- list.files(
    file.path(dir_out, "ppt", "charts"),
    pattern = "^chart(.*)\\.xml$",
    full.names = TRUE
  )
  attr(dir_out, "chart_xml") <- chart_xml
  dir_out
}
