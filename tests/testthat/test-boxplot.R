boxplot_data <- function() {
  set.seed(1)
  data.frame(
    group = rep(c("A", "B", "C"), each = 10),
    value = c(rnorm(10, 0, 5), rnorm(10, 3, 7), rnorm(10, -2, 4)),
    stringsAsFactors = FALSE
  )
}

test_that("ms_boxplotchart constructor validates inputs", {
  d <- boxplot_data()
  expect_error(ms_boxplotchart(d, x = "missing", y = "value"), "not found")
  expect_error(ms_boxplotchart(d, x = "group", y = "missing"), "not found")
  expect_error(ms_boxplotchart(d, x = "group", y = "group"), "must be numeric")
  expect_error(
    ms_boxplotchart(d, x = "group", y = "value", quartile_method = "bogus")
  )
})

test_that("ms_boxplotchart format produces well-formed cx XML", {
  bp <- ms_boxplotchart(
    boxplot_data(),
    x = "group",
    y = "value",
    quartile_method = "inclusive",
    show_mean_marker = TRUE,
    show_outliers = FALSE
  )
  xml_str <- format(bp, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "boxWhisker")

  vis <- xml2::xml_find_first(doc, "//cx:layoutPr/cx:visibility", ns)
  expect_equal(xml2::xml_attr(vis, "meanMarker"), "1")
  expect_equal(xml2::xml_attr(vis, "outliers"), "0")

  stats <- xml2::xml_find_first(doc, "//cx:layoutPr/cx:statistics", ns)
  expect_equal(xml2::xml_attr(stats, "quartileMethod"), "inclusive")

  axes <- xml2::xml_find_all(doc, "//cx:plotArea/cx:axis", ns)
  expect_length(axes, 2L)
})

test_that("ms_boxplotchart end-to-end pptx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  bp <- ms_boxplotchart(boxplot_data(), x = "group", y = "value")
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, bp, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})

test_that("boxplot series carries an <a:ln> by default (accent2)", {
  set.seed(1)
  bd <- data.frame(g = rep(c("A","B"), each = 10), v = rnorm(20))
  ms <- ms_boxplotchart(bd, x = "g", y = "v")
  doc <- xml2::read_xml(format(ms))
  ns <- c(
    cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
    a  = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )
  ln <- xml2::xml_find_first(
    doc, "//cx:series[@layoutId='boxWhisker']/cx:spPr/a:ln", ns
  )
  expect_false(inherits(ln, "xml_missing"))
  expect_equal(xml2::xml_attr(ln, "w"), "12700")
  scheme <- xml2::xml_find_first(ln, ".//a:schemeClr", ns)
  expect_equal(xml2::xml_attr(scheme, "val"), "accent2")
})

test_that("chart_settings(line = fp_border) overrides the boxplot stroke", {
  set.seed(1)
  bd <- data.frame(g = rep(c("A","B"), each = 10), v = rnorm(20))
  ms <- ms_boxplotchart(bd, x = "g", y = "v")
  ms <- chart_settings(ms, line = officer::fp_border(color = "red", width = 1.5))
  doc <- xml2::read_xml(format(ms))
  ns <- c(
    cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
    a  = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )
  ln <- xml2::xml_find_first(
    doc, "//cx:series[@layoutId='boxWhisker']/cx:spPr/a:ln", ns
  )
  expect_equal(xml2::xml_attr(ln, "w"), "19050")  # 1.5 pt * 12700
  srgb <- xml2::xml_find_first(ln, ".//a:srgbClr", ns)
  expect_equal(xml2::xml_attr(srgb, "val"), "FF0000")
})

test_that("chart_data_stroke wins over the default boxplot stroke (single spPr)", {
  set.seed(1)
  bd <- data.frame(g = rep(c("A","B"), each = 10), v = rnorm(20))
  ms <- ms_boxplotchart(bd, x = "g", y = "v")
  ms <- chart_data_stroke(ms, values = "blue", width = 1)
  doc <- xml2::read_xml(format(ms))
  ns <- c(
    cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
    a  = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )
  # Excel rejects two <cx:spPr> in a row -- the default must not be
  # injected when chart_data_stroke already produced one.
  spprs <- xml2::xml_find_all(
    doc, "//cx:series[@layoutId='boxWhisker']/cx:spPr", ns
  )
  expect_length(spprs, 1L)
  srgb <- xml2::xml_find_first(spprs, ".//a:ln//a:srgbClr", ns)
  expect_equal(xml2::xml_attr(srgb, "val"), "0000FF")
})

test_that("chart_settings(line = FALSE) drops the default boxplot stroke", {
  set.seed(1)
  bd <- data.frame(g = rep(c("A","B"), each = 10), v = rnorm(20))
  ms <- ms_boxplotchart(bd, x = "g", y = "v")
  ms <- chart_settings(ms, line = FALSE)
  doc <- xml2::read_xml(format(ms))
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")
  expect_true(inherits(
    xml2::xml_find_first(
      doc, "//cx:series[@layoutId='boxWhisker']/cx:spPr", ns
    ),
    "xml_missing"
  ))
})

test_that("chart_settings(line = ...) rejects bad values on boxplot", {
  bd <- data.frame(g = c("A","B"), v = c(1, 2))
  ms <- ms_boxplotchart(bd, x = "g", y = "v")
  expect_error(chart_settings(ms, line = "blue"), "fp_border")
  expect_error(chart_settings(ms, line = 1), "fp_border")
})
