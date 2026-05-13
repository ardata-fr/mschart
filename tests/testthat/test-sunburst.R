sunburst_data <- function() {
  data.frame(
    region = c("EU", "EU", "EU", "AM", "AM"),
    country = c("FR", "FR", "DE", "US", "US"),
    city = c("Paris", "Lyon", "Berlin", "NYC", "LA"),
    value = c(10, 5, 12, 20, 8),
    stringsAsFactors = FALSE
  )
}

test_that("ms_sunburstchart constructor validates inputs", {
  d <- sunburst_data()
  expect_error(
    ms_sunburstchart(d, path = c("region", "missing"), value = "value"),
    "not found in data"
  )
  expect_error(
    ms_sunburstchart(d, path = "region", value = "missing"),
    "not found in data"
  )
  expect_error(
    ms_sunburstchart(d, path = "region", value = "city"),
    "must be numeric"
  )
})

test_that("ms_sunburstchart format produces well-formed cx XML with layoutId=sunburst", {
  sb <- ms_sunburstchart(
    sunburst_data(),
    path = c("region", "country", "city"),
    value = "value"
  )

  xml_str <- format(sb, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  cat_lvls <- xml2::xml_find_all(doc, "//cx:strDim[@type='cat']/cx:lvl", ns)
  expect_length(cat_lvls, 3L)
  val_lvls <- xml2::xml_find_all(doc, "//cx:numDim[@type='size']/cx:lvl", ns)
  expect_length(val_lvls, 1L)

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "sunburst")

  # sunburst has no <cx:layoutPr> (unlike treemap)
  layout_pr <- xml2::xml_find_all(doc, "//cx:series/cx:layoutPr", ns)
  expect_length(layout_pr, 0L)
})

test_that("ms_sunburstchart can be inserted in a pptx end-to-end", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  sb <- ms_sunburstchart(
    sunburst_data(),
    path = c("region", "country", "city"),
    value = "value"
  )
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, sb, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
