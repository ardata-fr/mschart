histogram_data <- function() {
  set.seed(1)
  data.frame(x = rnorm(80, mean = 12, sd = 4))
}

test_that("ms_histogramchart constructor validates inputs", {
  d <- histogram_data()
  expect_error(ms_histogramchart(d, value = "missing"), "not found")
  expect_error(
    ms_histogramchart(data.frame(g = letters[1:5]), value = "g"),
    "must be numeric"
  )
  expect_error(
    ms_histogramchart(d, value = "x", bin_count = 10, bin_width = 1),
    "either"
  )
})

test_that("ms_histogramchart format produces well-formed cx XML", {
  hi <- ms_histogramchart(
    histogram_data(),
    value = "x",
    bin_count = 10,
    interval_closed = "left"
  )
  xml_str <- format(hi, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  # No cat dim, only val dim
  cat_dims <- xml2::xml_find_all(doc, "//cx:strDim", ns)
  expect_length(cat_dims, 0L)
  val_dims <- xml2::xml_find_all(doc, "//cx:numDim[@type='val']", ns)
  expect_length(val_dims, 1L)

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "clusteredColumn")

  binning <- xml2::xml_find_first(doc, "//cx:layoutPr/cx:binning", ns)
  expect_equal(xml2::xml_attr(binning, "intervalClosed"), "l")
  bin_count <- xml2::xml_find_first(doc, "//cx:binning/cx:binCount", ns)
  expect_equal(xml2::xml_attr(bin_count, "val"), "10")

  axes <- xml2::xml_find_all(doc, "//cx:plotArea/cx:axis", ns)
  expect_length(axes, 2L)
})

test_that("ms_histogramchart end-to-end pptx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  hi <- ms_histogramchart(histogram_data(), value = "x", bin_width = 2)
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, hi, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
