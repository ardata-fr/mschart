cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

test_that("chart_ax_y(num_fmt=) emits <cx:numFmt formatCode=...> on val axis", {
  d <- data.frame(step = c("A", "B"), amount = c(1.5, 2.5))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_y(wf, num_fmt = "0.00")
  doc <- xml2::read_xml(format(wf))
  nf <- xml2::xml_find_first(doc, "//cx:axis[@id='1']/cx:numFmt", cx_ns)
  expect_equal(xml2::xml_attr(nf, "formatCode"), "0.00")
  expect_equal(xml2::xml_attr(nf, "sourceLinked"), "0")
})

test_that("chart_ax_x(num_fmt=) emits <cx:numFmt> on cat axis", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_x(wf, num_fmt = "@")
  doc <- xml2::read_xml(format(wf))
  nf <- xml2::xml_find_first(doc, "//cx:axis[@id='0']/cx:numFmt", cx_ns)
  expect_equal(xml2::xml_attr(nf, "formatCode"), "@")
})

test_that("chart_ax_y(major_grid=FALSE) hides val axis major gridlines", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_y(wf, major_grid = FALSE)
  doc <- xml2::read_xml(format(wf))
  expect_length(
    xml2::xml_find_all(doc, "//cx:axis[@id='1']/cx:majorGridlines", cx_ns),
    0L
  )
})

test_that("default: val axis keeps major gridlines (back-compat)", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  doc <- xml2::read_xml(format(wf))
  expect_length(
    xml2::xml_find_all(doc, "//cx:axis[@id='1']/cx:majorGridlines", cx_ns),
    1L
  )
})

test_that("chart_ax_y(minor_grid=TRUE) shows minor gridlines", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_y(wf, minor_grid = TRUE)
  doc <- xml2::read_xml(format(wf))
  expect_length(
    xml2::xml_find_all(doc, "//cx:axis[@id='1']/cx:minorGridlines", cx_ns),
    1L
  )
})

test_that("chart_ax_y(limit_min=, limit_max=) embeds attributes on valScaling", {
  d <- data.frame(step = c("A", "B"), amount = c(1, 2))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_y(wf, limit_min = -10, limit_max = 200)
  doc <- xml2::read_xml(format(wf))
  vs <- xml2::xml_find_first(doc, "//cx:axis[@id='1']/cx:valScaling", cx_ns)
  expect_equal(xml2::xml_attr(vs, "min"), "-10")
  expect_equal(xml2::xml_attr(vs, "max"), "200")
})

test_that("chart_ax_y options work on histogram and pareto", {
  set.seed(1)
  hi <- ms_histogramchart(data.frame(x = rnorm(50)), value = "x")
  hi <- chart_ax_y(hi, num_fmt = "0", major_grid = FALSE)
  doc <- xml2::read_xml(format(hi))
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(doc, "//cx:axis[@id='1']/cx:numFmt", cx_ns),
      "formatCode"
    ),
    "0"
  )
  expect_length(
    xml2::xml_find_all(doc, "//cx:axis[@id='1']/cx:majorGridlines", cx_ns),
    0L
  )

  d <- data.frame(defect = c("A", "B"), n = c(10, 5))
  pa <- ms_paretochart(d, x = "defect", y = "n", aggregate = FALSE)
  pa <- chart_ax_y(pa, num_fmt = "#,##0")
  doc2 <- xml2::read_xml(format(pa))
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(doc2, "//cx:axis[@id='1']/cx:numFmt", cx_ns),
      "formatCode"
    ),
    "#,##0"
  )
  # axis 2 (percentage) is fixed and not affected
  expect_length(
    xml2::xml_find_all(doc2, "//cx:axis[@id='2']/cx:numFmt", cx_ns),
    0L
  )
})

test_that("chart_ax_x options work on funnel single axis", {
  d <- data.frame(stage = c("A", "B"), count = c(100, 50))
  fn <- ms_funnelchart(d, x = "stage", y = "count")
  fn <- chart_ax_x(fn, num_fmt = "@")
  doc <- xml2::read_xml(format(fn))
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(doc, "//cx:axis[@id='1']/cx:numFmt", cx_ns),
      "formatCode"
    ),
    "@"
  )
})

test_that("round-trip pptx still opens with axis options", {
  skip_if_not_installed("officer")
  d <- data.frame(step = c("A", "B", "C"), amount = c(10, -5, 8))
  wf <- ms_waterfallchart(d, x = "step", y = "amount")
  wf <- chart_ax_y(
    wf,
    num_fmt = "0",
    limit_min = -10,
    limit_max = 50,
    major_grid = TRUE,
    minor_grid = TRUE
  )
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, "Title and Content", "Office Theme")
  doc <- officer::ph_with(doc, wf, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})
