cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

# --- defaults preserved per layout (back-compat with Steps 0..4) ---------

test_that("default visibility: treemap shows category, hides val", {
  d <- data.frame(city = c("A", "B"), value = c(10, 20))
  tm <- ms_treemapchart(d, path = "city", value = "value")
  doc <- xml2::read_xml(format(tm))
  vis <- xml2::xml_find_first(doc, "//cx:dataLabels/cx:visibility", cx_ns)
  expect_equal(xml2::xml_attr(vis, "categoryName"), "1")
  expect_equal(xml2::xml_attr(vis, "value"), "0")
})

test_that("default visibility: waterfall shows val, hides cat", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  doc <- xml2::read_xml(format(wf))
  vis <- xml2::xml_find_first(doc, "//cx:dataLabels/cx:visibility", cx_ns)
  expect_equal(xml2::xml_attr(vis, "value"), "1")
  expect_equal(xml2::xml_attr(vis, "categoryName"), "0")
})

test_that("default visibility: boxplot/histogram/pareto emit no <cx:dataLabels>", {
  set.seed(1)
  bp <- ms_boxplotchart(
    data.frame(g = rep(c("A", "B"), each = 5), v = rnorm(10)),
    x = "g",
    y = "v"
  )
  hi <- ms_histogramchart(data.frame(x = rnorm(20)), value = "x")
  pa <- ms_paretochart(
    data.frame(d = c("A", "B"), n = c(5, 3)),
    x = "d",
    y = "n",
    aggregate = FALSE
  )
  for (chart in list(bp, hi, pa)) {
    doc <- xml2::read_xml(format(chart))
    expect_length(xml2::xml_find_all(doc, "//cx:dataLabels", cx_ns), 0L)
  }
})

# --- chart_data_labels overrides -----------------------------------------

test_that("chart_data_labels overrides treemap defaults (val on, cat off)", {
  d <- data.frame(city = c("A", "B"), value = c(10, 20))
  tm <- ms_treemapchart(d, path = "city", value = "value")
  tm <- chart_data_labels(tm, show_val = TRUE, show_cat = FALSE)
  doc <- xml2::read_xml(format(tm))
  vis <- xml2::xml_find_first(doc, "//cx:dataLabels/cx:visibility", cx_ns)
  expect_equal(xml2::xml_attr(vis, "value"), "1")
  expect_equal(xml2::xml_attr(vis, "categoryName"), "0")
})

test_that("chart_data_labels enables labels on histogram (none by default)", {
  set.seed(1)
  hi <- ms_histogramchart(data.frame(x = rnorm(20)), value = "x")
  hi <- chart_data_labels(hi, show_val = TRUE, position = "outEnd")
  doc <- xml2::read_xml(format(hi))
  dl <- xml2::xml_find_first(doc, "//cx:dataLabels", cx_ns)
  expect_equal(xml2::xml_attr(dl, "pos"), "outEnd")
  vis <- xml2::xml_find_first(dl, "./cx:visibility", cx_ns)
  expect_equal(xml2::xml_attr(vis, "value"), "1")
})

test_that("chart_data_labels(num_fmt=) emits <cx:numFmt> inside dataLabels", {
  d <- data.frame(step = c("A", "B"), amount = c(1.5, 2.5))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_data_labels(wf, num_fmt = "0.00")
  doc <- xml2::read_xml(format(wf))
  nf <- xml2::xml_find_first(doc, "//cx:dataLabels/cx:numFmt", cx_ns)
  expect_equal(xml2::xml_attr(nf, "formatCode"), "0.00")
})

test_that("chart_data_labels(position=) overrides default position", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_data_labels(wf, position = "ctr")
  doc <- xml2::read_xml(format(wf))
  dl <- xml2::xml_find_first(doc, "//cx:dataLabels", cx_ns)
  expect_equal(xml2::xml_attr(dl, "pos"), "ctr")
})

# --- chart_labels_text font ----------------------------------------------

test_that("chart_labels_text adds <cx:txPr> inside dataLabels with sz/color", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_labels_text(
    wf,
    officer::fp_text(font.size = 9, bold = TRUE, color = "purple")
  )
  doc <- xml2::read_xml(format(wf))
  defRPr <- xml2::xml_find_first(
    doc,
    "//cx:dataLabels/cx:txPr//a:defRPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(defRPr, "sz"), "900")
  expect_equal(xml2::xml_attr(defRPr, "b"), "1")
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(defRPr, ".//a:srgbClr", cx_ns), "val"),
    "A020F0"
  )
})

test_that("chart_labels_text errors on non-fp_text input", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  expect_error(
    chart_labels_text(wf, list(font.size = 10)),
    "single fp_text"
  )
})

# --- round-trip ---------------------------------------------------------

test_that("round-trip pptx still opens with chart_data_labels + chart_labels_text", {
  skip_if_not_installed("officer")
  d <- data.frame(step = c("A", "B", "C"), amount = c(10, -5, 8))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_data_labels(
    wf,
    show_val = TRUE,
    num_fmt = "0",
    position = "outEnd"
  )
  wf <- chart_labels_text(wf, officer::fp_text(font.size = 8, bold = TRUE))
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
