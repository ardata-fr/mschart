set.seed(1)
weather <- data.frame(
  temperature_c = runif(20, 5, 30),
  pressure_hpa  = runif(20, 990, 1030),
  humidity_t    = runif(20, 30, 90),
  humidity_p    = runif(20, 30, 90)
)

build_combo <- function() {
  sc_temp <- ms_scatterchart(
    data = weather, x = "temperature_c", y = "humidity_t"
  )
  sc_pres <- ms_scatterchart(
    data = weather, x = "pressure_hpa", y = "humidity_p"
  )
  ms_chart_combine(
    temp = sc_temp, pres = sc_pres, secondary_x = "pres"
  )
}

format_combo_xml <- function(combo) {
  xml <- format(combo, sheetname = "sheet1",
                id_x = "64451712", id_y = "64453248")
  xml2::read_xml(xml)
}

test_that("independent_x keeps both x columns in data_series", {
  cb <- build_combo()
  expect_true(all(c("temperature_c", "pressure_hpa") %in%
                    names(cb$data_series)))
  expect_equal(cb$xvar, "temperature_c")
  expect_equal(cb$secondary[[1]]$xvar, "pressure_hpa")
})

test_that("independent_x produces unique series idx/order", {
  doc <- format_combo_xml(build_combo())
  idx <- as.integer(xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:ser/c:idx"), "val"
  ))
  order <- as.integer(xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:ser/c:order"), "val"
  ))
  expect_length(idx, 2L)
  expect_equal(anyDuplicated(idx), 0L)
  expect_equal(anyDuplicated(order), 0L)
})

test_that("independent_x emits a top secondary x axis and 4 distinct axIds", {
  doc <- format_combo_xml(build_combo())
  ax_ids <- xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:plotArea/*/c:axId"), "val"
  )
  expect_equal(length(unique(ax_ids)), 4L)
  axpos <- xml2::xml_attr(
    xml2::xml_find_all(doc, "//c:plotArea/*/c:axPos"), "val"
  )
  expect_true("t" %in% axpos)
  expect_true("b" %in% axpos)
})

test_that("independent_x: primary and secondary reference disjoint columns", {
  doc <- format_combo_xml(build_combo())
  refs <- xml2::xml_text(xml2::xml_find_all(doc, "//c:f"))
  cols <- unique(sub("\\$.*$", "", sub("^.*!\\$", "", refs)))
  expect_true(all(c("A", "B", "C", "D") %in% cols))
})

test_that("ms_chart_combine errors on y column collisions in independent_x", {
  weather2 <- weather
  names(weather2)[names(weather2) == "humidity_p"] <- "humidity_t"
  sc_temp <- ms_scatterchart(weather, x = "temperature_c", y = "humidity_t")
  sc_pres <- ms_scatterchart(weather2, x = "pressure_hpa", y = "humidity_t")
  expect_error(
    ms_chart_combine(temp = sc_temp, pres = sc_pres, secondary_x = "pres"),
    "same column name"
  )
})

test_that("ms_chart_combine errors when a name is in both secondary_x and _y", {
  sc_temp <- ms_scatterchart(weather, x = "temperature_c", y = "humidity_t")
  sc_pres <- ms_scatterchart(weather, x = "pressure_hpa", y = "humidity_p")
  expect_error(
    ms_chart_combine(temp = sc_temp, pres = sc_pres,
                     secondary_x = "pres", secondary_y = "pres"),
    "cannot be on both"
  )
})

test_that("ms_chart_combine errors when mixing secondary_y and secondary_x", {
  sc_a <- ms_scatterchart(weather, x = "temperature_c", y = "humidity_t")
  sc_b <- ms_scatterchart(weather, x = "temperature_c", y = "humidity_p")
  sc_c <- ms_scatterchart(weather, x = "pressure_hpa",  y = "humidity_p")
  expect_error(
    ms_chart_combine(a = sc_a, b = sc_b, c = sc_c,
                     secondary_y = "b", secondary_x = "c"),
    "not supported"
  )
})

test_that("ms_chart_combine errors on multiple secondary_x charts", {
  sc1 <- ms_scatterchart(weather, x = "temperature_c", y = "humidity_t")
  sc2 <- ms_scatterchart(weather, x = "pressure_hpa", y = "humidity_p")
  sc3 <- ms_scatterchart(weather, x = "pressure_hpa", y = "humidity_t")
  expect_error(
    ms_chart_combine(a = sc1, b = sc2, c = sc3,
                     secondary_x = c("b", "c")),
    "Only one chart can be placed on secondary_x"
  )
})
