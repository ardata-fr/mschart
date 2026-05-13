waterfall_data <- function() {
  data.frame(
    step = c("Start", "Q1", "Q2", "Q3", "End"),
    amount = c(100, 30, -20, 40, 150),
    stringsAsFactors = FALSE
  )
}

test_that("ms_waterfallchart constructor validates inputs", {
  d <- waterfall_data()
  expect_error(ms_waterfallchart(d, x = "missing", y = "amount"), "not found")
  expect_error(ms_waterfallchart(d, x = "step", y = "missing"), "not found")
  expect_error(ms_waterfallchart(d, x = "step", y = "step"), "must be numeric")
  expect_error(
    ms_waterfallchart(d, x = "step", y = "amount", subtotals = c(1, 99)),
    "between 1 and"
  )
})

test_that("ms_waterfallchart format produces well-formed cx XML", {
  wf <- ms_waterfallchart(
    waterfall_data(),
    x = "step",
    y = "amount",
    subtotals = c(1, 5)
  )
  xml_str <- format(wf, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  expect_equal(xml2::xml_name(xml2::xml_root(doc)), "chartSpace")

  cat_lvls <- xml2::xml_find_all(doc, "//cx:strDim[@type='cat']/cx:lvl", ns)
  expect_length(cat_lvls, 1L)
  val_lvls <- xml2::xml_find_all(doc, "//cx:numDim[@type='val']/cx:lvl", ns)
  expect_length(val_lvls, 1L)

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "waterfall")

  # subtotals: 1-based 1 and 5 -> 0-based 0 and 4
  idx_nodes <- xml2::xml_find_all(doc, "//cx:subtotals/cx:idx", ns)
  expect_length(idx_nodes, 2L)
  expect_equal(xml2::xml_attr(idx_nodes, "val"), c("0", "4"))

  # axes
  axes <- xml2::xml_find_all(doc, "//cx:plotArea/cx:axis", ns)
  expect_length(axes, 2L)
})

test_that("ms_waterfallchart end-to-end pptx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  wf <- ms_waterfallchart(
    waterfall_data(),
    x = "step",
    y = "amount",
    subtotals = c(1, 5)
  )
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
