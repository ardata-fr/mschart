skip_on_cran()

test_that("barchart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_barchart(
    data = browser_data, x = "browser",
    y = "value", group = "serie"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, chart, location = officer::ph_location_fullsize())
  expect_snapshot_doc(x = doc, name = "visual-barchart", engine = "testthat")
})

test_that("linechart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_linechart(
    data = browser_ts, x = "date",
    y = "freq", group = "browser"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, chart, location = officer::ph_location_fullsize())
  expect_snapshot_doc(x = doc, name = "visual-linechart", engine = "testthat")
})

test_that("areachart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_areachart(
    data = browser_ts, x = "date",
    y = "freq", group = "browser"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, chart, location = officer::ph_location_fullsize())
  expect_snapshot_doc(x = doc, name = "visual-areachart", engine = "testthat")
})

test_that("scatterchart visual snapshot", {
  skip_if_not_installed("doconv")
  skip_if_not(doconv::msoffice_available())
  require(doconv)
  local_edition(3L)

  chart <- ms_scatterchart(
    data = iris, x = "Sepal.Length",
    y = "Sepal.Width", group = "Species"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, chart, location = officer::ph_location_fullsize())
  expect_snapshot_doc(x = doc, name = "visual-scatterchart", engine = "testthat")
})
