cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

# --- title is styled by theme$main_title ----------------------------------

test_that("chart_theme: main_title font/color is consumed by treemap title", {
  dat <- data.frame(city = c("A", "B"), value = c(10, 20))
  tm <- ms_treemapchart(dat, path = "city", value = "value")
  tm <- chart_labels(tm, title = "My Title")
  th <- mschart_theme(
    main_title = officer::fp_text(font.size = 30, bold = TRUE, color = "red")
  )
  tm <- set_theme(tm, th)
  doc <- xml2::read_xml(format(tm))
  rPr <- xml2::xml_find_first(doc, "//cx:title//a:r/a:rPr", cx_ns)
  expect_equal(xml2::xml_attr(rPr, "sz"), "3000")
  expect_equal(xml2::xml_attr(rPr, "b"), "1")
  fill <- xml2::xml_find_first(rPr, ".//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "FF0000")
})

test_that("chart_theme: title default theme produces sz=2000, b=1 (mschart_theme default)", {
  dat <- data.frame(city = c("A", "B"), value = c(10, 20))
  tm <- ms_treemapchart(dat, path = "city", value = "value")
  tm <- chart_labels(tm, title = "Default")
  doc <- xml2::read_xml(format(tm))
  rPr <- xml2::xml_find_first(doc, "//cx:title//a:r/a:rPr", cx_ns)
  expect_equal(xml2::xml_attr(rPr, "sz"), "2000")
  expect_equal(xml2::xml_attr(rPr, "b"), "1")
})

test_that("chart_theme: title is omitted entirely when no title is set", {
  dat <- data.frame(city = c("A", "B"), value = c(10, 20))
  tm <- ms_treemapchart(dat, path = "city", value = "value")
  doc <- xml2::read_xml(format(tm))
  expect_length(xml2::xml_find_all(doc, "//cx:title", cx_ns), 0L)
})

# --- axis tick labels are styled by theme$axis_text_x / _y ---------------

test_that("chart_theme: axis_text_x font is consumed by waterfall x axis", {
  dat <- data.frame(step = c("A", "B", "C"), amount = c(1, 2, 3))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  th <- mschart_theme(
    axis_text_x = officer::fp_text(font.size = 9, italic = TRUE, color = "blue")
  )
  wf <- set_theme(wf, th)
  doc <- xml2::read_xml(format(wf))
  defRPr <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='0']/cx:txPr//a:defRPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(defRPr, "sz"), "900")
  expect_equal(xml2::xml_attr(defRPr, "i"), "1")
  fill <- xml2::xml_find_first(defRPr, ".//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "0000FF")
})

test_that("chart_theme: axis_text_y font is consumed by waterfall y axis", {
  dat <- data.frame(step = c("A", "B", "C"), amount = c(1, 2, 3))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  th <- mschart_theme(
    axis_text_y = officer::fp_text(font.size = 11, bold = TRUE)
  )
  wf <- set_theme(wf, th)
  doc <- xml2::read_xml(format(wf))
  defRPr <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='1']/cx:txPr//a:defRPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(defRPr, "sz"), "1100")
  expect_equal(xml2::xml_attr(defRPr, "b"), "1")
})

test_that("chart_theme: pareto axis 2 (percentage) inherits axis_text_y", {
  d <- data.frame(defect = c("A", "B", "C"), n = c(10, 5, 3))
  pa <- ms_paretochart(d, x = "defect", y = "n", aggregate = FALSE)
  th <- mschart_theme(
    axis_text_y = officer::fp_text(font.size = 8, color = "purple")
  )
  pa <- set_theme(pa, th)
  doc <- xml2::read_xml(format(pa))
  defRPr2 <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='2']/cx:txPr//a:defRPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(defRPr2, "sz"), "800")
  fill <- xml2::xml_find_first(defRPr2, ".//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "A020F0")
})

test_that("chart_theme: funnel single axis id=1 uses axis_text_x", {
  d <- data.frame(stage = c("A", "B"), count = c(100, 50))
  fn <- ms_funnelchart(d, x = "stage", y = "count")
  th <- mschart_theme(axis_text_x = officer::fp_text(font.size = 7))
  fn <- set_theme(fn, th)
  doc <- xml2::read_xml(format(fn))
  defRPr <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='1']/cx:txPr//a:defRPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(defRPr, "sz"), "700")
})

# --- round-trip pptx ------------------------------------------------------

test_that("chart_theme: round-trip pptx still opens with custom title+axes theme", {
  skip_if_not_installed("officer")
  dat <- data.frame(step = c("A", "B", "C"), amount = c(10, -5, 8))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  wf <- chart_labels(wf, title = "Themed")
  th <- mschart_theme(
    main_title = officer::fp_text(
      font.size = 28,
      bold = TRUE,
      color = "darkgreen"
    ),
    axis_text_x = officer::fp_text(font.size = 9, italic = TRUE),
    axis_text_y = officer::fp_text(font.size = 9, color = "gray40")
  )
  wf <- set_theme(wf, th)
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
