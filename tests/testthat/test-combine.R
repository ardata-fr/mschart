browser_data <- data.frame(
  browser = rep(
    c("Android", "Chrome", "IE", "Firefox", "Opera", "Safari"),
    each = 3
  ),
  serie = rep(c("v1", "v2", "v3"), times = 6),
  value = c(
    80,
    90,
    77,
    150,
    140,
    131,
    60,
    70,
    62,
    50,
    60,
    52,
    40,
    50,
    42,
    55,
    65,
    55
  )
)
target <- data.frame(
  browser = unique(browser_data$browser),
  cible = 220
)

format_combo_xml <- function(combo) {
  xml <- format(
    combo,
    sheetname = "sheet1",
    id_x = "64451712",
    id_y = "64453248"
  )
  xml2::read_xml(xml)
}

test_that("ms_chart_combine assigns unique series idx/order across all charts", {
  bars <- ms_barchart(
    browser_data,
    x = "browser",
    y = "value",
    group = "serie"
  ) |>
    as_bar_stack()
  line <- ms_linechart(target, x = "browser", y = "cible")

  combo <- ms_chart_combine(bars = bars, target = line)
  doc <- format_combo_xml(combo)

  idx <- as.integer(xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:ser/c:idx"),
    "val"
  ))
  order <- as.integer(xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:ser/c:order"),
    "val"
  ))

  expect_length(idx, 4L)
  expect_equal(anyDuplicated(idx), 0L)
  expect_equal(anyDuplicated(order), 0L)
})

test_that("ms_chart_combine merges secondary data into the embedded sheet", {
  bars <- ms_barchart(
    browser_data,
    x = "browser",
    y = "value",
    group = "serie"
  ) |>
    as_bar_stack()
  line <- ms_linechart(target, x = "browser", y = "cible")

  combo <- ms_chart_combine(bars = bars, target = line)

  expect_true("cible" %in% names(combo$data_series))
  expect_equal(nrow(combo$data_series), nrow(target))
  expect_equal(combo$data_series$cible, rep(220, nrow(target)))

  doc <- format_combo_xml(combo)
  line_val <- xml2::xml_text(
    xml2::xml_find_first(doc, "//c:lineChart/c:ser/c:val/c:numRef/c:f")
  )
  bar_vals <- xml2::xml_text(
    xml2::xml_find_all(doc, "//c:barChart/c:ser/c:val/c:numRef/c:f")
  )
  expect_false(line_val %in% bar_vals)
})

test_that("ms_chart_combine errors on column-name collision between charts", {
  d1 <- data.frame(x = letters[1:3], y = 1:3)
  d2 <- data.frame(x = letters[1:3], y = 4:6)
  c1 <- ms_barchart(d1, x = "x", y = "y")
  c2 <- ms_linechart(d2, x = "x", y = "y")

  expect_error(ms_chart_combine(bars = c1, line = c2), "column name")
})

test_that("ms_chart_combine errors on x-value mismatch", {
  bars <- ms_barchart(
    browser_data,
    x = "browser",
    y = "value",
    group = "serie"
  ) |>
    as_bar_stack()
  line_bad_x <- ms_linechart(
    data.frame(browser = c("Android", "Chrome", "Other"), cible = 220),
    x = "browser",
    y = "cible"
  )

  expect_error(ms_chart_combine(bars = bars, target = line_bad_x), "x values")
})
