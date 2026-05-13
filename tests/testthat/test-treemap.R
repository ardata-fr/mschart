treemap_data <- function() {
  data.frame(
    region = c("EU", "EU", "EU", "AM", "AM"),
    country = c("FR", "FR", "DE", "US", "US"),
    city = c("Paris", "Lyon", "Berlin", "NYC", "LA"),
    value = c(10, 5, 12, 20, 8),
    stringsAsFactors = FALSE
  )
}

test_that("ms_treemapchart constructor validates inputs", {
  d <- treemap_data()

  expect_error(
    ms_treemapchart(d, path = c("region", "missing"), value = "value"),
    "not found in data"
  )
  expect_error(
    ms_treemapchart(d, path = "region", value = "missing"),
    "not found in data"
  )
  expect_error(
    ms_treemapchart(d, path = "region", value = "city"),
    "must be numeric"
  )
})

test_that("ms_treemapchart format produces well-formed cx XML", {
  tm <- ms_treemapchart(
    treemap_data(),
    path = c("region", "country", "city"),
    value = "value"
  )
  tm <- chart_labels(tm, title = "Sales by region")

  xml_str <- format(tm, sheetname = "sheet1")
  expect_true(nchar(xml_str) > 0)

  doc <- xml2::read_xml(xml_str)
  ns <- c(cx = "http://schemas.microsoft.com/office/drawing/2014/chartex")

  expect_equal(xml2::xml_name(xml2::xml_root(doc)), "chartSpace")

  # one <cx:data> with one strDim (cat) of 3 levels and one numDim (val)
  cat_lvls <- xml2::xml_find_all(doc, "//cx:strDim[@type='cat']/cx:lvl", ns)
  expect_length(cat_lvls, 3L)

  val_lvls <- xml2::xml_find_all(doc, "//cx:numDim[@type='size']/cx:lvl", ns)
  expect_length(val_lvls, 1L)

  # value level has 5 points
  pts <- xml2::xml_find_all(val_lvls[[1]], "./cx:pt", ns)
  expect_length(pts, 5L)

  # series with treemap layout
  series <- xml2::xml_find_all(doc, "//cx:series", ns)
  expect_length(series, 1L)
  expect_equal(xml2::xml_attr(series[[1]], "layoutId"), "treemap")

  # Office orders levels leaf-first: lvl[0] = city, lvl[2] = region
  leaf_pts <- xml2::xml_find_all(cat_lvls[[1]], "./cx:pt", ns)
  expect_equal(xml2::xml_text(leaf_pts[[1]]), "Paris")
  root_pts <- xml2::xml_find_all(cat_lvls[[3]], "./cx:pt", ns)
  expect_equal(xml2::xml_text(root_pts[[1]]), "EU")

  # title injected
  ttl <- xml2::xml_find_first(doc, "//cx:title", ns)
  expect_false(inherits(ttl, "xml_missing"))
})

test_that("ms_treemapchart can be inserted in a pptx end-to-end", {
  skip_if_not_installed("officer")
  skip_if_not_installed("writexl")

  tm <- ms_treemapchart(
    treemap_data(),
    path = c("region", "country", "city"),
    value = "value"
  )

  doc <- officer::read_pptx()
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "Office Theme"
  )
  doc <- officer::ph_with(doc, tm, location = officer::ph_location_fullsize())

  out <- tempfile(fileext = ".pptx")
  expect_silent(print(doc, target = out))
  expect_true(file.exists(out))

  # Inspect the produced package: chart part exists, content-type and
  # relationship use the chartEx URIs.
  unz_dir <- tempfile()
  dir.create(unz_dir)
  utils::unzip(out, exdir = unz_dir)

  chart_files <- list.files(
    file.path(unz_dir, "ppt", "charts"),
    pattern = "^chart.*\\.xml$",
    full.names = TRUE
  )
  expect_length(chart_files, 1L)
  chart_xml <- xml2::read_xml(chart_files[1])
  expect_equal(xml2::xml_name(xml2::xml_root(chart_xml)), "chartSpace")
  expect_match(
    xml2::xml_ns(chart_xml)[["cx"]],
    "office/drawing/2014/chartex",
    fixed = TRUE
  )

  ct_xml <- xml2::read_xml(file.path(unz_dir, "[Content_Types].xml"))
  ct_overrides <- xml2::xml_attr(
    xml2::xml_find_all(ct_xml, "//*[local-name()='Override']"),
    "ContentType"
  )
  expect_true(any(grepl("ms-office.chartex\\+xml", ct_overrides)))
})
