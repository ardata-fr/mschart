# XLSX property matrix for chartEx layouts (cx:).
#
# Same idea as examples/series_property_matrix-xlsx.R, but for the new
# chartEx layouts: treemap, sunburst, waterfall, funnel, boxplot,
# histogram, pareto. One sheet per (chart_type, property) combination,
# each sheet containing:
#   - A1 onwards : the chart data (written asis once, the two charts
#                  share it via write_data = FALSE)
#   - left chart : baseline (no custom property applied)
#   - right chart: same chart with the property under test applied
#   - below      : a commentary cell describing the expected difference
#
# Sheet names encode the combination (e.g. "waterfall - ax_y").

library(mschart)
library(officer)

# -- Datasets (one per layout) --------------------------------------------

treemap_data <- data.frame(
  region = c("A", "A", "B", "B", "C", "C"),
  city   = c("a1", "a2", "b1", "b2", "c1", "c2"),
  value  = c(40, 25, 30, 18, 22, 12),
  stringsAsFactors = FALSE
)

sunburst_data <- treemap_data

waterfall_data <- data.frame(
  step   = c("Start", "Q1", "Q2", "Q3", "End"),
  amount = c(100, 30, -20, 40, 150),
  stringsAsFactors = FALSE
)

funnel_data <- data.frame(
  stage = c("Visitors", "Leads", "Opportunities", "Quotes", "Customers"),
  count = c(5000, 4000, 3000, 1000, 250),
  stringsAsFactors = FALSE
)

boxplot_data <- data.frame(
  group = rep(c("g1", "g2", "g3"), each = 20),
  value = c(rnorm(20, 10, 2), rnorm(20, 12, 3), rnorm(20, 8, 1.5)),
  stringsAsFactors = FALSE
)

histogram_data <- data.frame(
  x = rnorm(120, mean = 50, sd = 10)
)

pareto_data <- data.frame(
  defect = c("A", "B", "C", "D", "E"),
  n      = c(40, 25, 15, 12, 8),
  stringsAsFactors = FALSE
)

# -- Baseline chart constructors ------------------------------------------

baseline_chart <- function(type) {
  switch(type,
    treemap   = ms_treemapchart(treemap_data,
                                path = c("region", "city"), value = "value"),
    sunburst  = ms_sunburstchart(sunburst_data,
                                 path = c("region", "city"), value = "value"),
    waterfall = ms_waterfallchart(waterfall_data, x = "step", y = "amount",
                                  subtotals = c(1, 5)),
    funnel    = ms_funnelchart(funnel_data, x = "stage", y = "count"),
    boxplot   = ms_boxplotchart(boxplot_data, x = "group", y = "value"),
    histogram = ms_histogramchart(histogram_data, value = "x", bin_count = 10),
    pareto    = ms_paretochart(pareto_data, x = "defect", y = "n",
                               aggregate = FALSE)
  )
}

# -- Chart data written asis to A1 (write_data = FALSE on chart calls) ----
# chartEx layouts hardcode their cell refs to columns A/B starting at row 2,
# so data must be put at (col 1, row 1) verbatim.

asis_data <- function(type) {
  switch(type,
    treemap   = ,
    sunburst  = treemap_data,
    waterfall = waterfall_data,
    funnel    = funnel_data,
    boxplot   = boxplot_data,
    histogram = histogram_data,
    pareto    = pareto_data
  )
}

# -- Property matrix (only properties meaningful to each layout) ----------

supported <- list(
  treemap   = c("fill", "stroke", "theme", "title", "data_labels", "labels_fp"),
  sunburst  = c("fill", "stroke", "theme", "title", "data_labels", "labels_fp"),
  waterfall = c("fill", "stroke", "theme", "title", "data_labels", "labels_fp",
                "ax_x", "ax_y"),
  funnel    = c("fill", "stroke", "theme", "title", "data_labels", "labels_fp",
                "ax_x"),
  boxplot   = c("fill", "stroke", "theme", "title", "ax_x", "ax_y"),
  histogram = c("fill", "stroke", "theme", "title", "data_labels",
                "ax_x", "ax_y"),
  pareto    = c("fill", "stroke", "theme", "title", "ax_x", "ax_y")
)

# -- Apply a property under test ------------------------------------------
# chartEx layouts each have a single series, so fill/stroke take a single
# unnamed colour that paints every point uniformly. labels_fp takes a
# single fp_text (chart_labels_text.ms_chart_ex signature).

apply_prop <- function(ch, type, prop) {
  switch(prop,
    fill        = chart_data_fill(ch, values = "#1B9E77"),
    stroke      = chart_data_stroke(ch, values = "#000000", width = 2.5),
    theme       = set_theme(
      chart_labels(ch, title = sprintf("%s -- themed", type)),
      mschart_theme(
        main_title  = fp_text(font.size = 22, bold = TRUE, color = "#1B4F72"),
        axis_text_x = fp_text(font.size = 9,  italic = TRUE, color = "#444444"),
        axis_text_y = fp_text(font.size = 9,  color = "#444444")
      )
    ),
    title       = chart_labels(ch, title = sprintf("%s -- styled", type)),
    data_labels = chart_data_labels(ch, show_val = TRUE, position = "ctr"),
    labels_fp   = chart_data_labels(
      chart_labels_text(ch, values = fp_text(
        font.size = 11, italic = TRUE, color = "#1F618D"
      )),
      show_val = TRUE, position = "ctr"
    ),
    ax_x        = chart_ax_x(ch,
      num_fmt = "0",
      major_grid = list(stroke = "#BDBDBD", width = 0.75, dash = "dash")
    ),
    ax_y        = chart_ax_y(ch,
      num_fmt = "0",
      major_grid = list(stroke = "#BDBDBD", width = 0.75, dash = "dash")
    )
  )
}

expected_text <- function(prop) {
  switch(prop,
    fill        = "Attendu : remplissage vert teal sur la serie stylee.",
    stroke      = "Attendu : contour noir 2.5pt visible sur la serie stylee.",
    theme       = paste("Attendu : titre 22pt gras bleu fonce,",
                        "axes 9pt gris (italique en X)."),
    title       = "Attendu : titre present a droite, absent a gauche.",
    data_labels = paste("Attendu : valeurs affichees au centre des elements",
                        "(la police se regle via labels_fp / chart_labels_text)."),
    labels_fp   = paste("Attendu : etiquettes en bleu italique 11pt,",
                        "valeurs visibles."),
    ax_x        = "Attendu : grille majeure X grise pointillee, format 0.",
    ax_y        = "Attendu : grille majeure Y grise pointillee, format 0."
  )
}

# -- Sheet placement ------------------------------------------------------

baseline_anchor <- list(left = 3, top = 0.5, width = 5, height = 3.5)
styled_anchor   <- list(left = 9, top = 0.5, width = 5, height = 3.5)
comment_row     <- 23L
comment_col     <- 5L

add_check_sheet <- function(wb, type, prop) {
  label <- sprintf("%s - %s", type, prop)
  wb <- add_sheet(wb, label = label)

  # 1) write the dataset asis once at A1
  wb <- sheet_write_data(wb, value = asis_data(type), sheet = label,
                         start_col = 1L, start_row = 1L)

  # 2) baseline + styled charts referring to the same data
  base_ch   <- baseline_chart(type)
  styled_ch <- apply_prop(baseline_chart(type), type, prop)

  wb <- sheet_add_drawing(wb, value = base_ch, sheet = label,
    write_data = FALSE,
    start_col = 1L, start_row = 1L,
    left   = baseline_anchor$left,   top    = baseline_anchor$top,
    width  = baseline_anchor$width,  height = baseline_anchor$height
  )
  wb <- sheet_add_drawing(wb, value = styled_ch, sheet = label,
    write_data = FALSE,
    start_col = 1L, start_row = 1L,
    left   = styled_anchor$left,   top    = styled_anchor$top,
    width  = styled_anchor$width,  height = styled_anchor$height
  )

  # 3) commentary cell
  commentary <- data.frame(Expected = expected_text(prop),
                           stringsAsFactors = FALSE)
  wb <- sheet_write_data(wb, value = commentary, sheet = label,
                         start_row = comment_row, start_col = comment_col)

  wb
}

# -- Build xlsx -----------------------------------------------------------

set.seed(1)
wb <- read_xlsx()

for (type in names(supported)) {
  for (prop in supported[[type]]) {
    message(sprintf("building sheet: %s - %s", type, prop))
    wb <- add_check_sheet(wb, type, prop)
  }
}

out <- file.path(getwd(), "chartex-series_property_matrix.xlsx")
print(wb, target = out)
message("Created: ", out)
if (interactive()) browseURL(out)
