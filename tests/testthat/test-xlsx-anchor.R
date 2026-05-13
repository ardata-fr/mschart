xdr_ns <- c(
  xdr = "http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"
)

bar_chart <- function() {
  ms_barchart(
    data = data.frame(
      x = c("A", "B", "C"),
      y = c(1, 3, 2),
      g = rep("s", 3)
    ),
    x = "x",
    y = "y",
    group = "g"
  )
}

# Build a one-shot xlsx with a chart anchored as requested, then return the
# parsed drawing1.xml so tests can inspect anchor shape and cell coordinates.
write_and_read_drawing <- function(...) {
  x <- officer::read_xlsx()
  x <- officer::add_sheet(x, label = "s")
  x <- officer::sheet_add_drawing(
    x,
    value = bar_chart(),
    sheet = "s",
    ...
  )
  out <- tempfile(fileext = ".xlsx")
  print(x, target = out)
  pkg <- tempfile()
  dir.create(pkg)
  officer::unpack_folder(out, folder = pkg)
  drw <- list.files(
    file.path(pkg, "xl", "drawings"),
    pattern = "\\.xml$",
    full.names = TRUE
  )[1]
  xml2::read_xml(drw)
}

test_that("anchor = \"B2:H20\" produces a twoCellAnchor", {
  doc <- write_and_read_drawing(anchor = "B2:H20")
  anchor <- xml2::xml_find_first(doc, "//xdr:twoCellAnchor", xdr_ns)
  expect_false(inherits(anchor, "xml_missing"))
  expect_equal(xml2::xml_attr(anchor, "editAs"), "twoCell")
  expect_equal(
    xml2::xml_text(
      xml2::xml_find_first(anchor, "xdr:from/xdr:col", xdr_ns)
    ),
    "1"
  )
  expect_equal(
    xml2::xml_text(
      xml2::xml_find_first(anchor, "xdr:to/xdr:col", xdr_ns)
    ),
    "7"
  )
  expect_equal(
    xml2::xml_text(
      xml2::xml_find_first(anchor, "xdr:to/xdr:row", xdr_ns)
    ),
    "19"
  )
})

test_that("anchor single cell produces a oneCellAnchor with width/height", {
  doc <- write_and_read_drawing(anchor = "C4", width = 5, height = 3)
  anchor <- xml2::xml_find_first(doc, "//xdr:oneCellAnchor", xdr_ns)
  expect_false(inherits(anchor, "xml_missing"))
  expect_equal(
    xml2::xml_text(
      xml2::xml_find_first(anchor, "xdr:from/xdr:col", xdr_ns)
    ),
    "2"
  )
  ext <- xml2::xml_find_first(anchor, "xdr:ext", xdr_ns)
  expect_equal(xml2::xml_attr(ext, "cx"), as.character(5 * 914400))
  expect_equal(xml2::xml_attr(ext, "cy"), as.character(3 * 914400))
})

test_that("no anchor keeps absoluteAnchor (legacy behaviour)", {
  doc <- write_and_read_drawing(left = 1, top = 1, width = 6, height = 4)
  expect_false(inherits(
    xml2::xml_find_first(doc, "//xdr:absoluteAnchor", xdr_ns),
    "xml_missing"
  ))
})

test_that("edit_as is passed through to the twoCellAnchor", {
  doc <- write_and_read_drawing(anchor = "B2:F10", edit_as = "oneCell")
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(doc, "//xdr:twoCellAnchor", xdr_ns),
      "editAs"
    ),
    "oneCell"
  )
})

test_that("invalid anchor values are rejected with a clear message", {
  x <- officer::read_xlsx()
  x <- officer::add_sheet(x, label = "s")
  expect_error(
    officer::sheet_add_drawing(
      x,
      value = bar_chart(),
      sheet = "s",
      anchor = 42
    ),
    "anchor.*single string"
  )
  expect_error(
    officer::sheet_add_drawing(
      x,
      value = bar_chart(),
      sheet = "s",
      anchor = "B2:"
    ),
    "FROM:TO"
  )
  expect_error(
    officer::sheet_add_drawing(
      x,
      value = bar_chart(),
      sheet = "s",
      anchor = "ZZ"
    ),
    "invalid cell reference"
  )
})
