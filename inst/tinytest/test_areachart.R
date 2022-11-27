library(mschart)
library(officer)
library(xml2)

# example areachart -------
chart_01 <- ms_areachart(data = browser_ts, x = "date", y = "freq", group = "browser")
chart_01 <- chart_ax_x(chart_01, num_fmt = "m/d/yy", rotation = -90)
chart_01 <- chart_data_labels(chart_01, show_val = TRUE)
chart_01 <- chart_labels_text( chart_01, values = fp_text(font.size = 7, color = "red") )

xml <- format(
  chart_01,
  sheetname = "sheet1",
  id_x = "64451212",
  id_y = "64453248"
)

chart <- read_xml(xml)
dLblPos <- xml_find_all(chart, "//c:ser/c:dLbls/c:dLblPos")

# test that no dLblPos is found in the series
expect_true(length(dLblPos) < 1)

if (require("doconv")) {
  using(doconv)
  chart_01 <- ms_areachart(data = browser_ts, x = "date", y = "freq", group = "browser")
  chart_01 <- chart_ax_x(chart_01, num_fmt = "m/d/yy", rotation = -90)
  pptx_file <- tempfile(fileext = ".pptx")
  doc <- read_pptx()
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = chart_01, location = ph_location_fullsize())
  print(doc, target = pptx_file)
  expect_snapshot_doc(x = pptx_file,name = "areachart-data-labels", engine = "tinytest")

}
