# Visual verification of .series_property_support
#
# Generates one slide per (chart_type, property) combination declared
# as supported in R/chart_data_styles.R. Each slide displays:
#   - left  : baseline chart (no custom series styling)
#   - right : same chart with the property under test applied
#   - below : description of the expected visual difference
#
# Walk the pptx slide by slide: if the two charts look identical for
# a given slide, the corresponding cell in .series_property_support
# is wrongly declared supported.

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
  pie     = "labels_fp"
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

# Baseline chart per type --------------------------------------------------
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

# Series names per type (order matters for named vectors) -----------------
series_names <- function(type) {
  switch(type,
    bar = , line = , area = , radar = ,
    scatter = , bubble = c("s1", "s2", "s3"),
    stock   = c("high", "low", "close"),
    pie     = "y"
  )
}

# Contrasted values per property ------------------------------------------
# `type` is used to flip smooth: line defaults to smooth (so we apply 0),
# scatter defaults to polyline (so we apply 1) -- contrast is reversed.
prop_values <- function(prop, s, type = NULL) {
  n <- length(s)
  switch(prop,
    fill       = setNames(c("#E66101", "#5E3C99", "#1B9E77")[seq_len(n)], s),
    colour     = setNames(c("#000000", "#FFFFFF", "#888888")[seq_len(n)], s),
    symbol     = setNames(c("circle", "triangle", "diamond")[seq_len(n)], s),
    size       = setNames(c(5, 15, 25)[seq_len(n)], s),
    line_width = setNames(c(1, 3, 6)[seq_len(n)], s),
    line_style = setNames(c("solid", "dashed", "dotted")[seq_len(n)], s),
    smooth     = setNames(rep(if (identical(type, "scatter")) 1 else 0, n), s),
    labels_fp  = {
      fps <- list(
        fp_text(bold = TRUE, color = "red", font.size = 14),
        fp_text(italic = TRUE, color = "blue", font.size = 10),
        fp_text(color = "darkgreen", font.size = 18)
      )
      if (n == 1) setNames(fps[1], s)
      else setNames(fps[seq_len(n)], s)
    }
  )
}

# Apply one property -------------------------------------------------------
apply_prop <- function(ch, type, prop) {
  s <- series_names(type)
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
  # labels need to be shown to be visible
  if (prop == "labels_fp") {
    ch <- chart_data_labels(ch, show_val = TRUE, position = "ctr")
  }
  # make stroke visible on filled shapes
  if (prop == "colour" && type %in% c("bar", "area", "bubble", "pie")) {
    lw <- setNames(rep(3, length(s)), s)
    ch <- chart_data_line_width(ch, values = lw)
  }
  # isolate fill by neutralising the default stroke on filled shapes
  # (default stroke uses the palette otherwise and pollutes the visual)
  if (prop == "fill" && type %in% c("bar", "area", "bubble")) {
    ch <- chart_data_stroke(ch,
      values = setNames(rep("transparent", length(s)), s))
  }
  # for line-like charts, stroke IS the line -- align stroke with the
  # fill values so each series line matches its marker fill colour
  if (prop == "fill" && type %in% c("line", "scatter", "radar")) {
    ch <- chart_data_stroke(ch, values = v)
  }
  # force a black stroke so the varying line_width is visibly distinct
  # on filled shapes (default stroke would tint with the fill palette)
  if (prop == "line_width" && type %in% c("bar", "area", "bubble", "pie")) {
    ch <- chart_data_stroke(ch,
      values = setNames(rep("#000000", length(s)), s))
  }
  ch
}

# Expected-visual description per property --------------------------------
expected_text <- function(prop) {
  switch(prop,
    fill       = paste(
      "Attendu : serie 1 = orange (#E66101), serie 2 = violet (#5E3C99),",
      "serie 3 = teal (#1B9E77).",
      "Palette choisie hors des couleurs par defaut de mschart.",
      "Si les trois series ont la meme couleur que la baseline,",
      "fill n'est pas applique."
    ),
    colour     = paste(
      "Attendu : contour serie 1 = noir, serie 2 = blanc, serie 3 = gris.",
      "line_width force a 3 pour rendre le contour visible.",
      "Si les contours restent par defaut, colour (stroke) n'est pas applique."
    ),
    symbol     = paste(
      "Attendu : serie 1 = cercles, serie 2 = triangles, serie 3 = losanges.",
      "Meme forme partout = symbol n'est pas applique."
    ),
    size       = paste(
      "Attendu : serie 1 petite (5), serie 2 moyenne (15), serie 3 grande (25).",
      "Tailles egales = size n'est pas applique."
    ),
    line_width = paste(
      "Attendu : serie 1 fine (1), serie 2 epaisse (3),",
      "serie 3 tres epaisse (6).",
      "Epaisseurs egales = line_width n'est pas applique.",
      "(scatter masque la ligne si line_width < 1.)"
    ),
    line_style = paste(
      "Attendu : serie 1 solide, serie 2 tirets (dashed), serie 3 pointilles (dotted).",
      "Toutes solides = line_style n'est pas applique."
    ),
    smooth     = paste(
      "Line : baseline lissee par defaut, attendu a droite =",
      "3 polylignes droites (smooth = 0).",
      "Scatter : baseline en polylignes par defaut, attendu a droite =",
      "3 courbes lissees (smooth = 1).",
      "Forme identique a la baseline = smooth n'est pas applique."
    ),
    labels_fp  = paste(
      "Attendu : etiquettes serie 1 = rouge gras 14pt,",
      "serie 2 = bleu italique 10pt, serie 3 = vert fonce 18pt.",
      "Styles d'etiquettes identiques = labels_fp n'est pas applique.",
      "(Pour pie : une seule serie, seul le style de s1 est visible.)"
    )
  )
}

# Slide builder ------------------------------------------------------------
add_check_slide <- function(doc, type, prop) {
  base <- baseline_chart(type)
  styled <- apply_prop(baseline_chart(type), type, prop)

  doc <- add_slide(doc, layout = "Title and Content",
                   master = "Office Theme")

  # title
  doc <- ph_with(doc,
                 value = sprintf("%s - %s", type, prop),
                 location = ph_location_type("title"))

  # left label + chart
  doc <- ph_with(doc, "Baseline (par defaut)",
                 location = ph_location(
                   left = 0.3, top = 1.4, width = 4.6, height = 0.3))
  doc <- ph_with(doc, base,
                 location = ph_location(
                   left = 0.3, top = 1.8, width = 4.6, height = 3.8))

  # right label + chart
  doc <- ph_with(doc, sprintf("Avec %s applique", prop),
                 location = ph_location(
                   left = 5.1, top = 1.4, width = 4.6, height = 0.3))
  doc <- ph_with(doc, styled,
                 location = ph_location(
                   left = 5.1, top = 1.8, width = 4.6, height = 3.8))

  # commentary
  doc <- ph_with(doc,
                 value = block_list(
                   fpar(ftext(expected_text(prop),
                              fp_text(font.size = 11)))
                 ),
                 location = ph_location(
                   left = 0.3, top = 5.8, width = 9.4, height = 1.4))

  doc
}

# Build pptx ---------------------------------------------------------------
doc <- read_pptx()

# cover slide
doc <- add_slide(doc, layout = "Title Slide", master = "Office Theme")
doc <- ph_with(doc,
               "Verification visuelle de .series_property_support",
               location = ph_location_type("ctrTitle"))
doc <- ph_with(doc,
               sprintf("%d slides - baseline vs. propriete appliquee",
                       sum(lengths(supported))),
               location = ph_location_type("subTitle"))

# one slide per supported (type, property)
for (type in names(supported)) {
  for (prop in supported[[type]]) {
    message(sprintf("building slide: %s - %s", type, prop))
    doc <- add_check_slide(doc, type, prop)
  }
}

out <- print(doc, target = tempfile(fileext = ".pptx"))
message("Created: ", out)
if (interactive()) browseURL(out)
