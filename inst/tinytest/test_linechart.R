library(mschart)
library(officer)
library(xml2)

dat <- data.frame(
  color=c(rep("green",3), rep("unclear",3), rep("gray",3)),
  musician=c(rep(c("Robert Wyatt", "John Zorn", "Damon Albarn"),3)),
  count = c(120, 101, 131, 200, 154, 187, 122, 197, 159),
  stringsAsFactors=F)


chart_01 <- ms_linechart(data = dat, x = "musician", y = "count", group = "color")

settings <- c(green = 1L, unclear = 0L, gray = 0L)
settings <- settings[order(names(settings))]
chart_01 <- chart_data_smooth(chart_01, values = settings )

xml <- format(
  chart_01,
  sheetname = "sheet1",
  id_x = "64451212",
  id_y = "64453248"
)

chart <- read_xml(xml)
serie_names <- xml_find_all(chart, "//c:ser/c:tx/c:strRef/c:strCache/c:pt/c:v")
serie_names <- xml_text(serie_names)
smooth_data <- xml_find_all(chart, "//c:smooth")
smooth_data <- as.integer(xml_attr(smooth_data, "val"))
names(smooth_data) <- serie_names
smooth_data <- smooth_data[order(names(smooth_data))]

expect_equivalent(settings, smooth_data)


if (require("doconv")) {
  using(doconv)
  pptx_file <- tempfile(fileext = ".pptx")
  doc <- read_pptx()
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = chart_01, location = ph_location_type(type = "body"))
  print(doc, target = pptx_file)

  expect_snapshot_doc(x = pptx_file,name = "linechart-data-smooth", engine = "tinytest")
}
