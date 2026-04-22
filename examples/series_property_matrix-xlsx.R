# XLSX sibling of examples/series_property_matrix.R
#
# Same visual verification of .series_property_support, but output as
# an Excel workbook. One sheet per (chart_type, property) combination,
# each sheet containing:
#   - left  : baseline chart (no custom series styling)
#   - right : same chart with the property under test applied
#   - below : commentary cell describing the expected visual difference
#
# Sheet names encode the combination (e.g. "bar - fill"). Scroll the
# tabs and compare the two charts side by side.

library(mschart)
library(officer)

# Supported matrix mirrors .series_property_support ------------------------
supported <- list(
  bar     = c("fill", "colour", "line_width", "labels_fp"),
  line    = c("fill", "colour", "symbol", "size", "line_width",
              "line_style", "smooth", "labels_fp"),
  area    = c("fill", "colour", "line_width", "labels_fp"),
  scatter = c("fill", "colour", "symbol", "size", "line_width",
              "line_style", "smooth", "labels_fp"),
  stock   = c("fill", "colour", "symbol", "size", "line_width", "line_style"),
  radar   = c("fill", "colour", "symbol", "size", "line_width",
              "line_style", "labels_fp"),
  bubble  = c("fill", "colour", "line_width"),
  pie     = c("fill", "colour", "line_width", "labels_fp")
)

# Test datasets (3 series where applicable) --------------------------------
multi_data <- data.frame(
  x = rep(c("A", "B", "C", "D", "E"), 3),
  y = c(3, 5, 4, 6, 4,  5, 3, 7, 4, 6,  4, 6, 3, 5, 7),
  grp = rep(c("s1", "s2", "s3"), each = 5)
)

scatter_data <- data.frame(
  x = rep(1:6, 3),
  y = c(2, 3, 5, 4, 6, 5,  3, 4, 3, 5, 4, 6,  4, 2, 4, 3, 5, 4),
  grp = rep(c("s1", "s2", "s3"), each = 6)
)

bubble_data <- data.frame(
  x = rep(1:5, 3),
  y = c(2, 3, 5, 4, 6,  3, 4, 3, 5, 4,  4, 2, 4, 3, 5),
  sz = c(5, 8, 6, 9, 7,  6, 9, 7, 8, 10,  8, 6, 9, 7, 8),
  grp = rep(c("s1", "s2", "s3"), each = 5)
)

stock_data <- data.frame(
  date = as.Date("2024-01-01") + 0:6,
  high = c(55, 57, 58, 60, 59, 58, 62),
  low = c(20, 22, 25, 21, 24, 23, 28),
  close = c(40, 45, 50, 42, 48, 46, 52)
)

pie_data <- data.frame(
  cat = c("A", "B", "C", "D"),
  y = c(40, 30, 20, 10)
)

baseline_chart <- function(type) {
  ch <- switch(type,
    bar     = ms_barchart(multi_data, x = "x", y = "y", group = "grp"),
    line    = chart_settings(
      ms_linechart(multi_data, x = "x", y = "y", group = "grp"),
      style = "lineMarker"
    ),
    area    = ms_areachart(multi_data, x = "x", y = "y", group = "grp"),
    scatter = chart_settings(
      ms_scatterchart(scatter_data, x = "x", y = "y", group = "grp"),
      style = "lineMarker"
    ),
    stock   = ms_stockchart(stock_data, x = "date",
                            high = "high", low = "low", close = "close"),
    radar   = ms_radarchart(multi_data, x = "x", y = "y", group = "grp"),
    bubble  = ms_bubblechart(bubble_data, x = "x", y = "y",
                             size = "sz", group = "grp"),
    pie     = ms_piechart(pie_data, x = "cat", y = "y")
  )
  ch
}

series_names <- function(type) {
  switch(type,
    bar = , line = , area = , radar = ,
    scatter = , bubble = c("s1", "s2", "s3"),
    stock   = c("high", "low", "close"),
    pie     = c("A", "B", "C", "D")
  )
}

prop_values <- function(prop, s, type = NULL) {
  n <- length(s)
  switch(prop,
    fill       = setNames(rep_len(c("#E66101", "#5E3C99", "#1B9E77",
                                    "#999999"), n), s),
    colour     = setNames(rep_len(c("#000000", "#FFFFFF",
                                    "#888888", "#444444"), n), s),
    symbol     = setNames(rep_len(c("circle", "triangle", "diamond"), n), s),
    size       = setNames(rep_len(c(5, 15, 25), n), s),
    line_width = setNames(rep_len(c(1, 3, 6), n), s),
    line_style = setNames(rep_len(c("solid", "dashed", "dotted"), n), s),
    smooth     = setNames(rep(if (identical(type, "scatter")) 1 else 0, n), s),
    labels_fp  = {
      fps <- list(
        fp_text(bold = TRUE, color = "red", font.size = 14),
        fp_text(italic = TRUE, color = "blue", font.size = 10),
        fp_text(color = "darkgreen", font.size = 18)
      )
      idx <- rep_len(seq_along(fps), n)
      setNames(fps[idx], s)
    }
  )
}

apply_prop <- function(ch, type, prop) {
  s <- series_names(type)
  # On pie, fill/colour/line_width are per-category (A/B/C/D) while
  # labels_fp is still per-series (the y-column name).
  if (type == "pie" && prop == "labels_fp") {
    s <- "y"
  }
  v <- prop_values(prop, s, type = type)
  fn <- switch(prop,
    fill       = chart_data_fill,
    colour     = chart_data_stroke,
    symbol     = chart_data_symbol,
    size       = chart_data_size,
    line_width = chart_data_line_width,
    line_style = chart_data_line_style,
    smooth     = chart_data_smooth,
    labels_fp  = chart_labels_text
  )
  ch <- fn(ch, values = v)
  if (prop == "labels_fp") {
    ch <- chart_data_labels(ch, show_val = TRUE, position = "ctr")
  }
  if (prop == "colour" && type %in% c("bar", "area", "bubble", "pie")) {
    lw <- setNames(rep(3, length(s)), s)
    ch <- chart_data_line_width(ch, values = lw)
  }
  if (prop == "fill" && type %in% c("bar", "area", "bubble")) {
    ch <- chart_data_stroke(ch,
      values = setNames(rep("transparent", length(s)), s))
  }
  if (prop == "fill" && type %in% c("line", "scatter", "radar")) {
    ch <- chart_data_stroke(ch, values = v)
  }
  if (prop == "line_width" && type %in% c("bar", "area", "bubble", "pie")) {
    ch <- chart_data_stroke(ch,
      values = setNames(rep("#000000", length(s)), s))
  }
  ch
}

expected_text <- function(prop) {
  switch(prop,
    fill       = paste(
      "Attendu : serie 1 = orange, serie 2 = violet, serie 3 = teal",
      "(palette hors defaut mschart). Meme couleur que baseline =",
      "fill n'est pas applique."
    ),
    colour     = paste(
      "Attendu : contours contrastes (noir / blanc / gris).",
      "line_width force a 3 pour rendre le contour visible."
    ),
    symbol     = "Attendu : cercles / triangles / losanges.",
    size       = "Attendu : tailles 5 / 15 / 25.",
    line_width = "Attendu : epaisseurs 1 / 3 / 6.",
    line_style = "Attendu : solide / tirets / pointilles.",
    smooth     = paste(
      "Line : baseline lissee, apply = polylignes droites.",
      "Scatter : baseline polylignes, apply = courbes lissees."
    ),
    labels_fp  = paste(
      "Attendu : etiquettes en rouge gras 14pt / bleu italique 10pt /",
      "vert fonce 18pt selon la serie.",
      "(Pour pie : styles par tranche A/B/C/D.)"
    )
  )
}

# Sheet-level data / chart placement --------------------------------------
# The chart XML hard-codes cell ranges at default position (col 1, row 1),
# so both charts must point to data written there. Baseline and styled
# share the same dataset; sheet_write_data is idempotent under overwrite.
# Geometry (Excel grid cells, 0-based for chart anchors):
#   - shared chart data : col  1,  row 1  (first 1-3 cols, rows 1-~20)
#   - baseline chart    : cols  5..14  x rows 1..20
#   - styled chart      : cols 16..25  x rows 1..20
#   - commentary cell   : col  5,  row 23
shared_data_col   <- 1L
shared_data_row   <- 1L
baseline_anchor   <- list(from_col = 5L,  from_row = 1L,
                          to_col   = 14L, to_row   = 20L)
styled_anchor     <- list(from_col = 16L, from_row = 1L,
                          to_col   = 25L, to_row   = 20L)
comment_row       <- 23L
comment_col       <- 5L

add_check_sheet <- function(wb, type, prop) {
  label <- sprintf("%s - %s", type, prop)
  # Excel sheet names <= 31 chars, no special chars -- our labels are fine
  wb <- add_sheet(wb, label = label)

  base_ch <- baseline_chart(type)
  styled_ch <- apply_prop(baseline_chart(type), type, prop)

  wb <- sheet_add_drawing(wb, value = base_ch, sheet = label,
    data_start_col = shared_data_col,
    data_start_row = shared_data_row,
    from_col = baseline_anchor$from_col,
    from_row = baseline_anchor$from_row,
    to_col   = baseline_anchor$to_col,
    to_row   = baseline_anchor$to_row
  )

  wb <- sheet_add_drawing(wb, value = styled_ch, sheet = label,
    data_start_col = shared_data_col,
    data_start_row = shared_data_row,
    from_col = styled_anchor$from_col,
    from_row = styled_anchor$from_row,
    to_col   = styled_anchor$to_col,
    to_row   = styled_anchor$to_row
  )

  # commentary row: a 1-column data.frame whose column name holds the
  # label "Expected" and whose single value holds the commentary text
  commentary <- data.frame(Expected = expected_text(prop),
                           stringsAsFactors = FALSE)
  wb <- sheet_write_data(wb, value = commentary, sheet = label,
    start_row = comment_row, start_col = comment_col)

  wb
}

# Build xlsx ---------------------------------------------------------------
wb <- read_xlsx()

for (type in names(supported)) {
  for (prop in supported[[type]]) {
    message(sprintf("building sheet: %s - %s", type, prop))
    wb <- add_check_sheet(wb, type, prop)
  }
}

out <- file.path(getwd(), "series_property_matrix.xlsx")
print(wb, target = out)
message("Created: ", out)
if (interactive()) browseURL(out)
