test_that("UTF-8 characters in chart titles and data", {
  test <- browser_data
  test[test$browser == "Opera", 1] <- "\U0001f47d"

  chart_01 <- ms_barchart(
    data = test,
    x = "browser",
    y = "value",
    group = "serie"
  )
  chart_01 <- chart_labels(chart_01, title = "\U0001f47d", xlab = "\U0001f47d")

  xml <- format(
    chart_01,
    sheetname = "sheet1",
    id_x = "64451212",
    id_y = "64453248"
  )

  chart <- xml2::read_xml(xml)
  txt <- xml2::xml_find_all(chart, "//c:title/c:tx/c:rich/a:p/a:r/a:t")

  expect_equal(
    xml2::xml_text(txt),
    c("\U0001f47d", "\U0001f47d"),
    ignore_attr = TRUE
  )

  txt_data <- xml2::xml_find_all(
    chart,
    "//c:ser[1]/c:cat/c:strRef/c:strCache/c:pt"
  )
  expect_equal(
    xml2::xml_text(txt_data),
    c("Android", "Chrome", "Firefox", "IE", "Safari", "\U0001f47d"),
    ignore_attr = TRUE
  )
})

test_that("num_fmt with XML metacharacters does not break chart XML (#98)", {
  fmt <- '[>=1000000]0.0,,"M";[>=1000]0.0,"K";0'

  chart <- ms_barchart(
    data = browser_data, x = "browser", y = "value", group = "serie"
  )
  chart <- chart_ax_y(chart, num_fmt = fmt)

  xml <- format(
    chart, sheetname = "sheet1",
    id_x = "64451212", id_y = "64453248"
  )

  doc <- xml2::read_xml(xml)
  code <- xml2::xml_attr(
    xml2::xml_find_first(doc, "//c:valAx/c:numFmt"),
    "formatCode"
  )
  expect_equal(code, fmt)
})
