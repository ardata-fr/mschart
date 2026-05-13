cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

bar_data <- function() {
  data.frame(
    step = c("Start", "Q1", "Q2", "Q3", "End"),
    amount = c(100, 30, -20, 40, 150)
  )
}

test_that("chart_data_stroke: single color emits per-point <a:ln> on waterfall", {
  # Waterfall keeps stroke per-point: Excel reuses series-level spPr/ln
  # for the connector lines between bars, so a uniform color is still
  # written as one <cx:dataPt> per row.
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  wf <- chart_data_stroke(wf, values = "black", width = 1.5)
  doc <- xml2::read_xml(format(wf))
  expect_length(
    xml2::xml_find_all(doc, "//cx:series/cx:spPr/a:ln", cx_ns),
    0L
  )
  lns <- xml2::xml_find_all(doc, "//cx:series/cx:dataPt/cx:spPr/a:ln", cx_ns)
  expect_length(lns, 5L)
  expect_true(all(
    xml2::xml_attr(lns, "w") == as.character(round(1.5 * 12700))
  ))
  fills <- xml2::xml_find_all(lns, ".//a:srgbClr", cx_ns)
  expect_true(all(xml2::xml_attr(fills, "val") == "000000"))
})

test_that("chart_data_stroke: named vector emits per-point dataPt with <a:ln>", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  wf <- chart_data_stroke(wf, values = c(Start = "red", End = "blue"))
  doc <- xml2::read_xml(format(wf))
  dpt <- xml2::xml_find_all(doc, "//cx:series/cx:dataPt", cx_ns)
  expect_length(dpt, 2L)
  expect_setequal(xml2::xml_attr(dpt, "idx"), c("0", "4"))
  end_color <- xml2::xml_attr(
    xml2::xml_find_first(
      dpt[xml2::xml_attr(dpt, "idx") == "4"],
      ".//a:ln//a:srgbClr",
      cx_ns
    ),
    "val"
  )
  expect_equal(end_color, "0000FF")
})

test_that("chart_data_fill + chart_data_stroke combine in same dataPt", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  wf <- chart_data_fill(wf, values = c(Q2 = "red"))
  wf <- chart_data_stroke(wf, values = c(Q2 = "black"))
  doc <- xml2::read_xml(format(wf))
  dpt_q2 <- xml2::xml_find_first(doc, "//cx:series/cx:dataPt[@idx='2']", cx_ns)
  fill <- xml2::xml_find_first(dpt_q2, ".//a:solidFill/a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "FF0000")
  ln <- xml2::xml_find_first(dpt_q2, ".//a:ln//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(ln, "val"), "000000")
})

test_that("chart_data_stroke: invalid color rejected", {
  wf <- ms_waterfallchart(bar_data(), x = "step", y = "amount")
  expect_error(chart_data_stroke(wf, values = "not-a-color"), "invalid color")
  expect_error(chart_data_stroke(wf, values = NULL), "non-empty")
})

test_that("chart_data_stroke: histogram rejects named-vector (no cat column)", {
  set.seed(1)
  hi <- ms_histogramchart(data.frame(x = rnorm(20)), value = "x")
  expect_error(
    chart_data_stroke(hi, values = c(A = "red")),
    "named-vector stroke"
  )
  hi2 <- chart_data_stroke(hi, values = "black")
  doc <- xml2::read_xml(format(hi2))
  expect_length(
    xml2::xml_find_all(doc, "//cx:series/cx:spPr/a:ln", cx_ns),
    1L
  )
})

test_that("round-trip pptx still opens with fill+stroke combo", {
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
  wf <- chart_data_stroke(wf, values = "black", width = 1)
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
