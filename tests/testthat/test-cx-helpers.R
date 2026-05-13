cx_ns <- c(
  cx = "http://schemas.microsoft.com/office/drawing/2014/chartex",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

# --- cx_color_hex (normalization helper, used by chart_data_fill) -------

test_that("cx_color_hex normalizes named, hex and #hex inputs", {
  expect_equal(cx_color_hex("red"), "FF0000")
  expect_equal(cx_color_hex("#00ff00"), "00FF00")
  expect_equal(cx_color_hex("0000FF"), "0000FF")
})

test_that("cx_color_hex returns NULL for empty/NA/transparent", {
  expect_null(cx_color_hex(NULL))
  expect_null(cx_color_hex(NA))
  expect_null(cx_color_hex(""))
  expect_null(cx_color_hex("transparent"))
})

test_that("cx_color_hex errors on invalid color string", {
  expect_error(cx_color_hex("not-a-color"), "invalid color")
})

# --- cx_solid_fill (delegates to officer::solid_fill) -------------------

test_that("cx_solid_fill: delegates to officer::solid_fill, includes alpha", {
  out <- cx_solid_fill("red")
  expect_match(out, "^<a:solidFill>")
  expect_match(out, "<a:srgbClr val=\"FF0000\">")
  expect_match(out, "<a:alpha val=\"100000\"/>")
})

test_that("cx_solid_fill: returns empty for NULL/NA/empty/transparent", {
  expect_equal(cx_solid_fill(NULL), "")
  expect_equal(cx_solid_fill(NA), "")
  expect_equal(cx_solid_fill(""), "")
  expect_equal(cx_solid_fill("transparent"), "")
})

# --- cx_line (delegates to officer::sp_line + to_pml) -------------------

test_that("cx_line: emits <a:ln w=> in EMU and wraps the color via solid_fill", {
  out <- cx_line("black", width = 1)
  expect_match(out, "^<a:ln w=\"12700\"")
  expect_match(out, "<a:srgbClr val=\"000000\">")
  expect_match(cx_line("blue", width = 0.75), "w=\"9525\"")
})

test_that("cx_line: returns empty for NULL/transparent", {
  expect_equal(cx_line(NULL), "")
  expect_equal(cx_line("transparent"), "")
})

test_that("cx_line: accepts a pre-built sp_line for advanced usage", {
  sp <- officer::sp_line(color = "red", lwd = 2, lty = "dash")
  out <- cx_line(sp_line = sp)
  expect_match(out, "w=\"25400\"")
  expect_match(out, "<a:prstDash val=\"dash\"/>")
})

# --- cx_spPr / cx_dataPt -----------------------------------------------

test_that("cx_spPr returns empty when no fill/line provided", {
  expect_equal(cx_spPr(), "")
  expect_equal(cx_spPr(fill = NULL, line_color = NULL), "")
})

test_that("cx_spPr combines fill and line", {
  out <- cx_spPr(fill = "red", line_color = "black", line_width = 1)
  expect_match(out, "^<cx:spPr>")
  expect_match(out, "</cx:spPr>$")
  expect_match(out, "srgbClr val=\"FF0000\"")
  expect_match(out, "<a:ln w=\"12700\"")
})

test_that("cx_dataPt wraps cx:spPr in <cx:dataPt idx=N>", {
  out <- cx_dataPt(2L, fill = "#123456")
  expect_match(out, "^<cx:dataPt idx=\"2\">")
  expect_match(out, "</cx:dataPt>$")
  expect_match(out, "srgbClr val=\"123456\"")
  expect_equal(cx_dataPt(0L), "") # no fill/line -> empty
})

# --- cx_rPr / cx_txPr / cx_rich_text via officer's format(fp, "pml") ---

test_that("cx_rPr_from_fp_text emits a complete <a:rPr> with sz/b/i/solidFill/latin", {
  fp <- officer::fp_text(
    font.size = 14,
    bold = TRUE,
    color = "red",
    font.family = "Calibri"
  )
  out <- cx_rPr_from_fp_text(fp)
  doc <- xml2::read_xml(paste0(
    "<root xmlns:cx=\"",
    cx_ns[["cx"]],
    "\" xmlns:a=\"",
    cx_ns[["a"]],
    "\">",
    out,
    "</root>"
  ))
  rpr <- xml2::xml_find_first(doc, "//a:rPr", cx_ns)
  expect_equal(xml2::xml_attr(rpr, "sz"), "1400")
  expect_equal(xml2::xml_attr(rpr, "b"), "1")
  fill <- xml2::xml_find_first(rpr, ".//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "FF0000")
  latin <- xml2::xml_find_first(rpr, ".//a:latin", cx_ns)
  expect_equal(xml2::xml_attr(latin, "typeface"), "Calibri")
})

test_that("cx_rPr_from_fp_text(as_def=TRUE) rewrites tag to <a:defRPr>", {
  fp <- officer::fp_text(font.size = 12)
  out <- cx_rPr_from_fp_text(fp, as_def = TRUE)
  expect_match(out, "<a:defRPr ")
  expect_false(grepl("<a:rPr ", out))
})

test_that("cx_rPr_from_fp_text(NULL) returns empty string", {
  expect_equal(cx_rPr_from_fp_text(NULL), "")
})

test_that("cx_txPr_from_fp_text wraps defRPr in a valid <cx:txPr>", {
  fp <- officer::fp_text(
    font.size = 12,
    bold = TRUE,
    color = "blue",
    font.family = "Calibri"
  )
  out <- cx_txPr_from_fp_text(fp)
  doc <- xml2::read_xml(paste0(
    "<root xmlns:cx=\"",
    cx_ns[["cx"]],
    "\" xmlns:a=\"",
    cx_ns[["a"]],
    "\">",
    out,
    "</root>"
  ))
  txPr <- xml2::xml_find_first(doc, "//cx:txPr", cx_ns)
  expect_false(inherits(txPr, "xml_missing"))
  defRPr <- xml2::xml_find_first(doc, "//a:defRPr", cx_ns)
  expect_equal(xml2::xml_attr(defRPr, "sz"), "1200")
  expect_equal(xml2::xml_attr(defRPr, "b"), "1")
  fill <- xml2::xml_find_first(defRPr, ".//a:srgbClr", cx_ns)
  expect_equal(xml2::xml_attr(fill, "val"), "0000FF")
})

test_that("cx_txPr_from_fp_text(NULL) returns empty string", {
  expect_equal(cx_txPr_from_fp_text(NULL), "")
})

test_that("cx_rich_text builds a valid <cx:tx><cx:rich> with rPr+text", {
  fp <- officer::fp_text(font.size = 18, bold = TRUE, color = "red")
  out <- cx_rich_text("Hello & <world>", fp = fp)
  doc <- xml2::read_xml(paste0(
    "<root xmlns:cx=\"",
    cx_ns[["cx"]],
    "\" xmlns:a=\"",
    cx_ns[["a"]],
    "\">",
    out,
    "</root>"
  ))
  rPr <- xml2::xml_find_first(doc, "//a:r/a:rPr", cx_ns)
  expect_equal(xml2::xml_attr(rPr, "sz"), "1800")
  expect_equal(xml2::xml_attr(rPr, "b"), "1")
  txt <- xml2::xml_find_first(doc, "//a:r/a:t", cx_ns)
  expect_equal(xml2::xml_text(txt), "Hello & <world>")
})

test_that("cx_rich_text without fp still emits a valid title fragment", {
  out <- cx_rich_text("plain title")
  expect_match(out, "<a:t>plain title</a:t>")
  expect_match(out, "<a:rPr/>")
})

test_that("chart_settings on chartEx types without options gives clear error", {
  funnel <- ms_funnelchart(
    data.frame(s = c("A", "B"), n = c(2, 1)),
    x = "s", y = "n"
  )
  expect_error(
    chart_settings(funnel),
    "chart_settings.*no options.*ms_funnelchart"
  )

  treemap <- ms_treemapchart(
    data.frame(g = c("a", "b"), v = c(1, 2)),
    path = "g", value = "v"
  )
  expect_error(
    chart_settings(treemap),
    "ms_treemapchart"
  )

  # pareto and boxplot still dispatch to their dedicated methods
  pa <- ms_paretochart(
    data.frame(x = c("A", "B"), y = c(2, 1)),
    x = "x", y = "y", aggregate = FALSE
  )
  expect_silent(chart_settings(pa, line = officer::fp_border(color = "red")))
})
