library(mschart)
library(officer)
library(xml2)

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

test <- browser_data
test[test$browser=='Opera', 1] <- '\U0001f47d'

chart_01 <- ms_barchart(data = test, x = "browser", y = "value", group = "serie")
chart_01 <- chart_labels(chart_01, title = "\U0001f47d", xlab = "\U0001f47d")

path <- unpack_chart(chart_01)

chart <- read_xml(attr(path, "chart_xml"))
txt <- xml_find_all(chart, "//c:title/c:tx/c:rich/a:p/a:r/a:t")

expect_equivalent(xml_text(txt), c("\U0001f47d", "\U0001f47d"))


txt_data <- xml_find_all(chart, "//c:ser[1]/c:cat/c:strRef/c:strCache/c:pt")
expect_equivalent(xml_text(txt_data), c("Android", "Chrome", "Firefox", "IE", "Safari", "\U0001f47d"))
