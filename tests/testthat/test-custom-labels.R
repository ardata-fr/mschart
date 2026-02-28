test_that("custom labels are correctly set in barchart", {
  dat_no_group <- data.frame(
    stringsAsFactors = FALSE,
    cut = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
    carat = c(1, 0.82, 0.71, 0.86, 0.54),
    n = c(1610L, 4906L, 12082L, 13791L, 21551L),
    label = c("\U0001f603", "\U0001f525", "", "Hello \U0001f47d", "\U0001f44e"),
    group = c("OK", "KO", "KO", "KO", "KO")
  )

  chrt <- ms_barchart(
    data = dat_no_group, group = "group",
    x = "cut", labels = "label", y = "n"
  )

  xml <- format(
    chrt,
    sheetname = "sheet1",
    id_x = "64451212",
    id_y = "64453248"
  )

  chart <- xml2::read_xml(xml)
  showDataLabelsRange <- xml2::xml_find_all(chart, "//c:ser/c:dLbls/c:extLst/c:ext/c15:showDataLabelsRange")

  expect_equal(xml2::xml_attr(showDataLabelsRange, "val"), c("1", "1"), ignore_attr = TRUE)

  label_pt <- xml2::xml_find_all(chart, "//c:ser/c:extLst/c:ext/c15:datalabelsRange/c15:dlblRangeCache/c:pt")
  expect_equal(
    xml2::xml_text(label_pt),
    c("", "\U0001f525", "\U0001f44e", "Hello \U0001f47d", "", "\U0001f603", "", "", "", ""),
    ignore_attr = TRUE
  )
})

test_that("custom minor/major units work", {

  scatter <- ms_scatterchart(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species")
  scatter <- chart_settings(scatter, scatterstyle = "marker")

  # default
  expect_equal(scatter$y_axis$major_unit, NULL)
  expect_equal(scatter$y_axis$minor_unit, NULL)

  # set values
  scatter <- chart_ax_x(scatter, major_tick_mark = "in", minor_tick_mark = "in", major_unit = 0.333, minor_unit = 1)
  scatter <- chart_ax_y(scatter, major_tick_mark = "in", minor_tick_mark = "in", major_unit = 0.333, minor_unit = 1)

  # values are available
  expect_equal(scatter$y_axis$major_unit, 0.333)
  expect_equal(scatter$y_axis$minor_unit, 1)

  expect_equal(scatter$x_axis$major_unit, 0.333)
  expect_equal(scatter$x_axis$minor_unit, 1)

  # removal
  scatter <- chart_ax_x(scatter, major_tick_mark = "in", minor_tick_mark = "in", major_unit = NULL, minor_unit = NULL)
  scatter <- chart_ax_y(scatter, major_tick_mark = "in", minor_tick_mark = "in", major_unit = NULL, minor_unit = NULL)

  expect_equal(scatter$y_axis$major_unit, NULL)
  expect_equal(scatter$y_axis$minor_unit, NULL)

})
