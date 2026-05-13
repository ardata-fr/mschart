funnel_data <- function() {
  data.frame(
    stage = c("Visitors", "Leads", "Opportunities", "Quotes", "Customers"),
    count = c(5000, 4000, 3000, 1000, 250),
    stringsAsFactors = FALSE
  )
}

test_that("ms_funnelchart constructor validates inputs", {
  d <- funnel_data()
  expect_error(ms_funnelchart(d, x = "missing", y = "count"), "not found")
  expect_error(ms_funnelchart(d, x = "stage", y = "missing"), "not found")
  expect_error(ms_funnelchart(d, x = "stage", y = "stage"), "must be numeric")
})

test_that("ms_funnelchart format produces well-formed cx XML", {
  fn <- ms_funnelchart(funnel_data(), x = "stage", y = "count")
  xml_str <- format(fn, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  cat_lvls <- xml2::xml_find_all(doc, "//cx:strDim[@type='cat']/cx:lvl", ns)
  expect_length(cat_lvls, 1L)
  val_lvls <- xml2::xml_find_all(doc, "//cx:numDim[@type='val']/cx:lvl", ns)
  expect_length(val_lvls, 1L)

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "funnel")

  # single axis id="1"
  axes <- xml2::xml_find_all(doc, "//cx:plotArea/cx:axis", ns)
  expect_length(axes, 1L)
  expect_equal(xml2::xml_attr(axes[[1]], "id"), "1")
})

test_that("ms_funnelchart end-to-end pptx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  fn <- ms_funnelchart(funnel_data(), x = "stage", y = "count")
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, fn, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
