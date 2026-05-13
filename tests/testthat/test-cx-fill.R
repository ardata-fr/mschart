cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

bar_data <- function() {
  data.frame(
    step = c("Start", "Q1", "Q2", "Q3", "End"),
    amount = c(100, 30, -20, 40, 150),
    stringsAsFactors = FALSE
  )
}
hier_data <- function() {
  data.frame(
    region = c("EU", "EU", "AM", "AM"),
    city = c("Paris", "Lyon", "NYC", "LA"),
    value = c(10, 5, 20, 8),
    stringsAsFactors = FALSE
  )
}

test_that("chart_data_fill: single color emits one <cx:spPr> on the series", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  wf <- chart_data_fill(wf, values = "#3366CC")
  doc <- xml2::read_xml(format(wf))
  spPr_nodes <- xml2::xml_find_all(
    doc,
    "//cx:series/cx:spPr/a:solidFill/a:srgbClr",
    cx_ns
  )
  expect_length(spPr_nodes, 1L)
  expect_equal(xml2::xml_attr(spPr_nodes, "val"), "3366CC")
  # No per-point dataPt emitted in single-color mode
  expect_length(xml2::xml_find_all(doc, "//cx:series/cx:dataPt", cx_ns), 0L)
})

test_that("chart_data_fill: named vector emits per-point <cx:dataPt> with correct idx", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  wf <- chart_data_fill(
    wf,
    values = c(Start = "gray", End = "gray", Q2 = "red")
  )
  doc <- xml2::read_xml(format(wf))
  dpt <- xml2::xml_find_all(doc, "//cx:series/cx:dataPt", cx_ns)
  expect_length(dpt, 3L)
  # 0-based idx: Start=0, Q2=2, End=4
  expect_setequal(xml2::xml_attr(dpt, "idx"), c("0", "2", "4"))
  # Q2 -> red FF0000
  q2_color <- xml2::xml_attr(
    xml2::xml_find_first(
      dpt[xml2::xml_attr(dpt, "idx") == "2"],
      ".//a:srgbClr",
      cx_ns
    ),
    "val"
  )
  expect_equal(q2_color, "FF0000")
})

test_that("chart_data_fill: full per-row vector applied positionally", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  cols <- c("#111111", "#222222", "#333333", "#444444", "#555555")
  wf <- chart_data_fill(wf, values = cols)
  doc <- xml2::read_xml(format(wf))
  dpt <- xml2::xml_find_all(doc, "//cx:series/cx:dataPt", cx_ns)
  expect_length(dpt, 5L)
  vals <- xml2::xml_attr(
    xml2::xml_find_all(
      doc,
      "//cx:series/cx:dataPt/cx:spPr/a:solidFill/a:srgbClr",
      cx_ns
    ),
    "val"
  )
  expect_equal(vals, c("111111", "222222", "333333", "444444", "555555"))
})

test_that("chart_data_fill: hierarchical chart matches against leaf column", {
  tm <- ms_treemapchart(
    hier_data(),
    path = c("region", "city"),
    value = "value"
  )
  tm <- chart_data_fill(tm, values = c(Paris = "blue", LA = "orange"))
  doc <- xml2::read_xml(format(tm))
  dpt <- xml2::xml_find_all(doc, "//cx:series/cx:dataPt", cx_ns)
  expect_length(dpt, 2L)
  expect_setequal(xml2::xml_attr(dpt, "idx"), c("0", "3"))
})

test_that("chart_data_fill: histogram rejects named-vector fill (no cat column)", {
  set.seed(1)
  hi <- ms_histogramchart(data.frame(x = rnorm(50)), value = "x")
  expect_error(
    chart_data_fill(hi, values = c(A = "red")),
    "named-vector fill is not supported"
  )
  # but single color works
  hi2 <- chart_data_fill(hi, values = "purple")
  doc <- xml2::read_xml(format(hi2))
  fill <- xml2::xml_find_first(
    doc,
    "//cx:series/cx:spPr/a:solidFill/a:srgbClr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(fill, "val"), "A020F0")
})

test_that("chart_data_fill: pareto with aggregate=TRUE rejects named-vector fill", {
  d <- data.frame(defect = sample(c("A", "B", "C"), 30, replace = TRUE))
  pa <- ms_paretochart(d, x = "defect")
  expect_error(
    chart_data_fill(pa, values = c(A = "red")),
    "named-vector fill is not supported"
  )
  # single color is fine and applies to the column series only
  pa2 <- chart_data_fill(pa, values = "#0080FF")
  doc <- xml2::read_xml(format(pa2))
  fills <- xml2::xml_find_all(
    doc,
    "//cx:series[@layoutId='clusteredColumn']/cx:spPr/a:solidFill/a:srgbClr",
    cx_ns
  )
  expect_length(fills, 1L)
})

test_that("chart_data_fill: invalid color rejected", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  expect_error(chart_data_fill(wf, values = "not-a-color"), "invalid color")
  expect_error(chart_data_fill(wf, values = NULL), "non-empty")
  expect_error(chart_data_fill(wf, values = 1L), "character vector")
})

test_that("chart_data_fill: unknown category name in named vector errors", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  expect_error(
    chart_data_fill(wf, values = c(Bogus = "red")),
    "not found in 'step'"
  )
})

test_that("chart_data_fill: round-trip pptx still opens (single color, treemap)", {
  skip_if_not_installed("officer")
  tm <- ms_treemapchart(
    hier_data(),
    path = c("region", "city"),
    value = "value"
  )
  tm <- chart_data_fill(tm, values = "#FF8800")
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, tm, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})

test_that("chart_data_fill: round-trip pptx still opens (per-point, waterfall)", {
  skip_if_not_installed("officer")
  wf <- ms_waterfallchart(
    bar_data(),
    x = "step",
    y = "amount",
    subtotals = c(1, 5)
  )
  wf <- chart_data_fill(
    wf,
    values = c(Start = "gray", End = "gray", Q2 = "red")
  )
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
