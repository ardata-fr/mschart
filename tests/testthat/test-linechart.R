test_that("linechart smooth settings are correctly applied", {
  dat <- data.frame(
    color = c(rep("green", 3), rep("unclear", 3), rep("gray", 3)),
    musician = c(rep(c("Robert Wyatt", "John Zorn", "Damon Albarn"), 3)),
    count = c(120, 101, 131, 200, 154, 187, 122, 197, 159),
    stringsAsFactors = FALSE
  )

  chart_01 <- ms_linechart(
    data = dat,
    x = "musician",
    y = "count",
    group = "color"
  )

  settings <- c(green = 1L, unclear = 0L, gray = 0L)
  settings <- settings[order(names(settings))]
  chart_01 <- chart_data_smooth(chart_01, values = settings)

  xml <- format(
    chart_01,
    sheetname = "sheet1",
    id_x = "64451212",
    id_y = "64453248"
  )

  chart <- xml2::read_xml(xml)
  serie_names <- xml2::xml_find_all(
    chart,
    "//c:ser/c:tx/c:strRef/c:strCache/c:pt/c:v"
  )
  serie_names <- xml2::xml_text(serie_names)
  smooth_data <- xml2::xml_find_all(chart, "//c:smooth")
  smooth_data <- as.integer(xml2::xml_attr(smooth_data, "val"))
  names(smooth_data) <- serie_names
  smooth_data <- smooth_data[order(names(smooth_data))]

  expect_equal(settings, smooth_data, ignore_attr = TRUE)
})

test_that("linechart style=none produces noFill XML", {
  dat <- data.frame(
    color = c(rep("green", 3), rep("unclear", 3), rep("gray", 3)),
    musician = c(rep(c("Robert Wyatt", "John Zorn", "Damon Albarn"), 3)),
    count = c(120, 101, 131, 200, 154, 187, 122, 197, 159),
    stringsAsFactors = FALSE
  )

  lty <- c(green = "none", unclear = "solid", gray = "dotted")
  lty <- lty[order(names(lty))]

  chart_02 <- ms_linechart(
    data = dat,
    x = "musician",
    y = "count",
    group = "color"
  )
  chart_02 <- chart_data_line_style(chart_02, values = lty)

  xml <- format(
    chart_02,
    sheetname = "sheet1",
    id_x = "64451212",
    id_y = "64453248"
  )

  chart <- xml2::read_xml(xml)

  # "none" → noFill, others → solidFill
  lookup_fill <- c(none = "noFill", solid = "solidFill", dotted = "solidFill")
  node_fill <- xml2::xml_find_all(
    chart,
    "//c:ser/c:spPr/a:ln/a:noFill|//c:ser/c:spPr/a:ln/a:solidFill"
  )
  node_fill <- xml2::xml_name(node_fill)
  expect_equal(node_fill, unname(lookup_fill[lty]))

  # prstDash: absent for "none", correct val for others
  lines <- xml2::xml_find_all(chart, "//c:ser/c:spPr/a:ln")
  lookup_lty <- c(none = NA_character_, solid = "solid", dotted = "sysDot")
  attr_lty <- vapply(
    lines,
    function(x) xml2::xml_attr(xml2::xml_find_first(x, "a:prstDash"), "val"),
    FUN.VALUE = character(1)
  )
  expect_equal(attr_lty, unname(lookup_lty[lty]))
})
