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
