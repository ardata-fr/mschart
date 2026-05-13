cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

test_that("axis title: xlab/ylab from chart_labels emit <cx:title> inside <cx:axis>", {
  dat <- data.frame(step = c("A", "B", "C"), amount = c(1, 2, 3))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  wf <- chart_labels(wf, xlab = "Stages", ylab = "Amount (USD)")
  doc <- xml2::read_xml(format(wf))

  x_title <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='0']/cx:title//a:t",
    cx_ns
  )
  expect_equal(xml2::xml_text(x_title), "Stages")

  y_title <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='1']/cx:title//a:t",
    cx_ns
  )
  expect_equal(xml2::xml_text(y_title), "Amount (USD)")
})

test_that("axis title: theme axis_title_x and _y are consumed", {
  dat <- data.frame(step = c("A", "B", "C"), amount = c(1, 2, 3))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  wf <- chart_labels(wf, xlab = "Stages", ylab = "Amount")
  th <- mschart_theme(
    axis_title_x = officer::fp_text(font.size = 14, bold = TRUE, color = "red"),
    axis_title_y = officer::fp_text(
      font.size = 12,
      italic = TRUE,
      color = "blue"
    )
  )
  wf <- set_theme(wf, th)
  doc <- xml2::read_xml(format(wf))

  x_rPr <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='0']/cx:title//a:r/a:rPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(x_rPr, "sz"), "1400")
  expect_equal(xml2::xml_attr(x_rPr, "b"), "1")
  expect_equal(
    xml2::xml_attr(xml2::xml_find_first(x_rPr, ".//a:srgbClr", cx_ns), "val"),
    "FF0000"
  )

  y_rPr <- xml2::xml_find_first(
    doc,
    "//cx:axis[@id='1']/cx:title//a:r/a:rPr",
    cx_ns
  )
  expect_equal(xml2::xml_attr(y_rPr, "sz"), "1200")
  expect_equal(xml2::xml_attr(y_rPr, "i"), "1")
})

test_that("axis title: NULL xlab/ylab emits no <cx:title> inside axis", {
  dat <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  doc <- xml2::read_xml(format(wf))
  axis_titles <- xml2::xml_find_all(doc, "//cx:axis/cx:title", cx_ns)
  expect_length(axis_titles, 0L)
})

test_that("axis title: funnel single axis honours xlab", {
  d <- data.frame(stage = c("A", "B"), count = c(100, 50))
  fn <- ms_funnelchart(d, x = "stage", y = "count")
  fn <- chart_labels(fn, xlab = "Funnel stage")
  doc <- xml2::read_xml(format(fn))
  t <- xml2::xml_find_first(doc, "//cx:axis[@id='1']/cx:title//a:t", cx_ns)
  expect_equal(xml2::xml_text(t), "Funnel stage")
})

test_that("axis title: pareto axes 0 and 1 honour xlab/ylab; axis 2 ignores", {
  d <- data.frame(defect = c("A", "B", "C"), n = c(10, 5, 3))
  pa <- ms_paretochart(d, x = "defect", y = "n", aggregate = FALSE)
  pa <- chart_labels(pa, xlab = "Defect type", ylab = "Count")
  doc <- xml2::read_xml(format(pa))
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(
      doc,
      "//cx:axis[@id='0']/cx:title//a:t",
      cx_ns
    )),
    "Defect type"
  )
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(
      doc,
      "//cx:axis[@id='1']/cx:title//a:t",
      cx_ns
    )),
    "Count"
  )
  # axis id=2 (percentage) has no title
  ax2_titles <- xml2::xml_find_all(doc, "//cx:axis[@id='2']/cx:title", cx_ns)
  expect_length(ax2_titles, 0L)
})

test_that("axis title: round-trip pptx still opens with axis titles", {
  skip_if_not_installed("officer")
  dat <- data.frame(step = c("A", "B", "C"), amount = c(10, -5, 8))
  wf <- ms_waterfallchart(dat, x = "step", y = "amount")
  wf <- chart_labels(wf, title = "T", xlab = "X axis", ylab = "Y axis")
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
