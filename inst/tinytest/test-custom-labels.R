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

dat_no_group <- data.frame(
  stringsAsFactors = FALSE,
  cut = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
  carat = c(1, 0.82, 0.71, 0.86, 0.54),
  n = c(1610L, 4906L, 12082L, 13791L, 21551L),
  label = c("\U0001f603", "\U0001f525", "", "Hello \U0001f47d", "\U0001f44e"),
  group = c("OK", "KO", "KO", "KO", "KO")
)

chrt <- ms_barchart(
  data = dat_no_group, group = "group",
  x = "cut", labels = "label", y = "n"
)


path <- unpack_chart(chrt)

chart <- read_xml(attr(path, "chart_xml"))
showDataLabelsRange <- xml_find_all(chart, "//c:ser/c:dLbls/c:extLst/c:ext/c15:showDataLabelsRange")

# test that no showDataLabelsRange is set to 1
expect_equivalent(xml_attr(showDataLabelsRange, "val"), c("1", "1"))

label_pt <- xml_find_all(chart, "//c:ser/c:extLst/c:ext/c15:datalabelsRange/c15:dlblRangeCache/c:pt")
expect_equivalent(xml_text(label_pt),
                  c("", "\U0001f525", "\U0001f44e", "Hello \U0001f47d", "", "\U0001f603", "", "", "", ""))


