pareto_data <- function() {
  set.seed(1)
  data.frame(
    defect = sample(
      c("A", "B", "C", "D"),
      50,
      replace = TRUE,
      prob = c(0.5, 0.25, 0.15, 0.1)
    ),
    stringsAsFactors = FALSE
  )
}

test_that("ms_paretochart constructor validates inputs", {
  d <- pareto_data()
  expect_error(ms_paretochart(d, x = "missing"), "not found")
  expect_error(ms_paretochart(d, x = "defect", y = "missing"), "not found")
  expect_error(ms_paretochart(d, x = "defect", y = "defect"), "must be numeric")
})

test_that("ms_paretochart format produces well-formed cx XML with two series and three axes", {
  pa <- ms_paretochart(pareto_data(), x = "defect")
  xml_str <- format(pa, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 2L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "clusteredColumn")
  expect_equal(xml2::xml_attr(series[[2]], "layoutId"), "paretoLine")
  expect_equal(xml2::xml_attr(series[[2]], "ownerIdx"), "0")

  # aggregation marker on the column series
  agg <- xml2::xml_find_all(
    doc,
    "//cx:series[@layoutId='clusteredColumn']/cx:layoutPr/cx:aggregation",
    ns
  )
  expect_length(agg, 1L)

  # 3 axes
  axes <- xml2::xml_find_all(doc, "//cx:plotArea/cx:axis", ns)
  expect_length(axes, 3L)
  pct_axis <- axes[[3]]
  unit_node <- xml2::xml_find_first(pct_axis, "./cx:units", ns)
  expect_equal(xml2::xml_attr(unit_node, "unit"), "percentage")
})

test_that("ms_paretochart aggregate=FALSE omits aggregation element", {
  d <- data.frame(defect = c("A", "B", "C"), n = c(20, 12, 8))
  pa <- ms_paretochart(d, x = "defect", y = "n", aggregate = FALSE)
  xml_str <- format(pa, sheetname = "sheet1")
  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")
  agg <- xml2::xml_find_all(doc, "//cx:aggregation", ns)
  expect_length(agg, 0L)
})

test_that("ms_paretochart end-to-end pptx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  pa <- ms_paretochart(pareto_data(), x = "defect")
  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, pa, location = officer::ph_location_fullsize())
  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))
})

test_that("paretoLine series carries an <a:ln> by default (accent2)", {
  pa <- ms_paretochart(
    data.frame(x = c("A", "B"), y = c(2, 1)),
    x = "x", y = "y", aggregate = FALSE
  )
  doc <- xml2::read_xml(format(pa))
  ns <- c(
    cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
    a  = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )
  ln <- xml2::xml_find_first(
    doc, "//cx:series[@layoutId='paretoLine']/cx:spPr/a:ln", ns
  )
  expect_false(inherits(ln, "xml_missing"))
  expect_equal(xml2::xml_attr(ln, "w"), "12700")
  scheme <- xml2::xml_find_first(ln, ".//a:schemeClr", ns)
  expect_equal(xml2::xml_attr(scheme, "val"), "accent2")
})

test_that("chart_settings(line = fp_border) overrides the pareto line", {
  pa <- ms_paretochart(
    data.frame(x = c("A", "B"), y = c(2, 1)),
    x = "x", y = "y", aggregate = FALSE
  )
  pa <- chart_settings(pa, line = officer::fp_border(color = "orange", width = 2))
  doc <- xml2::read_xml(format(pa))
  ns <- c(
    cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
    a  = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )
  ln <- xml2::xml_find_first(
    doc, "//cx:series[@layoutId='paretoLine']/cx:spPr/a:ln", ns
  )
  expect_equal(xml2::xml_attr(ln, "w"), "25400")  # 2 pt * 12700
  srgb <- xml2::xml_find_first(ln, ".//a:srgbClr", ns)
  expect_equal(xml2::xml_attr(srgb, "val"), "FFA500")
})

test_that("chart_settings(line = FALSE) suppresses the pareto line spPr", {
  pa <- ms_paretochart(
    data.frame(x = c("A", "B"), y = c(2, 1)),
    x = "x", y = "y", aggregate = FALSE
  )
  pa <- chart_settings(pa, line = FALSE)
  doc <- xml2::read_xml(format(pa))
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")
  expect_true(inherits(
    xml2::xml_find_first(
      doc, "//cx:series[@layoutId='paretoLine']/cx:spPr", ns
    ),
    "xml_missing"
  ))
})

test_that("chart_settings rejects bad `line` values", {
  pa <- ms_paretochart(
    data.frame(x = c("A", "B"), y = c(2, 1)),
    x = "x", y = "y", aggregate = FALSE
  )
  expect_error(chart_settings(pa, line = "blue"), "fp_border")
  expect_error(chart_settings(pa, line = 1), "fp_border")
})
