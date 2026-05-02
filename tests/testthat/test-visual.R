skip_on_cran()

test_that("barchart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_barchart(
    data = browser_data,
    x = "browser",
    y = "value",
    group = "serie"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(
    doc,
    chart,
    location = officer::ph_location_fullsize()
  )
  expect_snapshot_doc(x = doc, name = "visual-barchart", engine = "testthat")
})

test_that("linechart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_linechart(
    data = browser_ts,
    x = "date",
    y = "freq",
    group = "browser"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(
    doc,
    chart,
    location = officer::ph_location_fullsize()
  )
  expect_snapshot_doc(x = doc, name = "visual-linechart", engine = "testthat")
})

test_that("areachart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_areachart(
    data = browser_ts,
    x = "date",
    y = "freq",
    group = "browser"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(
    doc,
    chart,
    location = officer::ph_location_fullsize()
  )
  expect_snapshot_doc(x = doc, name = "visual-areachart", engine = "testthat")
})

test_that("scatterchart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_scatterchart(
    data = iris,
    x = "Sepal.Length",
    y = "Sepal.Width",
    group = "Species"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(
    doc,
    chart,
    location = officer::ph_location_fullsize()
  )
  expect_snapshot_doc(
    x = doc,
    name = "visual-scatterchart",
    engine = "testthat"
  )
})

test_that("combine bar+line visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  combine_data <- data.frame(
    browser = rep(c("Android", "Chrome", "Firefox", "IE", "Opera", "Safari"),
                  each = 3),
    serie   = rep(c("v1", "v2", "v3"), times = 6),
    value   = c(80, 90, 77, 150, 140, 131,
                50, 60, 52, 60, 70, 62,
                40, 50, 42, 55, 65, 55)
  )
  target <- data.frame(
    browser = unique(combine_data$browser),
    cible   = 220
  )

  bars <- ms_barchart(combine_data, x = "browser",
                      y = "value", group = "serie") |>
    as_bar_stack()

  line <- ms_linechart(target, x = "browser", y = "cible") |>
    chart_data_stroke(values = "#D62728") |>
    chart_data_line_width(values = 2) |>
    chart_data_symbol(values = "none")

  combo <- ms_chart_combine(bars = bars, target = line)

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content",
                            master = "Office Theme")
  doc <- officer::ph_with(doc, combo,
                          location = officer::ph_location_fullsize())
  expect_snapshot_doc(x = doc, name = "visual-combine-bar-line",
                      engine = "testthat")
})

test_that("pie visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  dat <- data.frame(
    browser = c("Chrome", "Firefox", "Safari", "Edge", "Other"),
    value = c(64, 12, 8, 5, 11)
  )

  # Pie chart
  pie <- ms_piechart(data = dat, x = "browser", y = "value")
  pie <- chart_labels(pie, title = "Browser share")

  # Doughnut chart
  donut <- ms_piechart(data = dat, x = "browser", y = "value")
  donut <- chart_settings(donut, hole_size = 50)
  donut <- chart_labels(donut, title = "Browser share (donut)")

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, pie, location = officer::ph_location_fullsize())
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(
    doc,
    donut,
    location = officer::ph_location_fullsize()
  )
  expect_snapshot_doc(x = doc, name = "visual-pie", engine = "testthat")
})
