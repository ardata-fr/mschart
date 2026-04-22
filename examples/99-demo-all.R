library(mschart)
library(officer)

doc <- read_pptx()

# -- slide 1: barchart (stacked, themed) ------------------------------------
chart_bar <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
chart_bar <- chart_settings(chart_bar,
  grouping = "stacked", gap_width = 80, overlap = 100, dir = "vertical"
)
chart_bar <- chart_data_fill(chart_bar,
  values = c(serie1 = "#4E79A7", serie2 = "#F28E2B", serie3 = "#E15759")
)
chart_bar <- chart_data_stroke(chart_bar,
  values = c(serie1 = "#3B5F87", serie2 = "#C06E1A", serie3 = "#B33E40")
)
chart_bar <- chart_labels(chart_bar,
  title = "Browser usage", xlab = "Browser", ylab = "Count"
)
chart_bar <- chart_ax_x(chart_bar,
  rotation = -45, tick_label_pos = "low"
)
chart_bar <- chart_ax_y(chart_bar,
  limit_min = 0, limit_max = 30, num_fmt = "0"
)
chart_bar <- chart_theme(chart_bar,
  grid_major_line_y = fp_border(color = "#DDDDDD", style = "dashed"),
  grid_major_line_x = fp_border(width = 0),
  axis_text_x = fp_text(font.size = 10, bold = TRUE),
  axis_text_y = fp_text(font.size = 9),
  plot_background = "#F9F9F9",
  legend_position = "b"
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_bar, location = ph_location_fullsize())


# -- slide 2: linechart (markers, smooth, labels text) ----------------------
chart_line <- ms_linechart(
  data = us_indus_prod, x = "date",
  y = "value", group = "type"
)
chart_line <- chart_settings(chart_line, style = "lineMarker")
chart_line <- chart_data_symbol(chart_line,
  values = c(unadjusted = "circle", adjusted = "diamond")
)
chart_line <- chart_data_size(chart_line,
  values = c(unadjusted = 4, adjusted = 5)
)
chart_line <- chart_data_stroke(chart_line,
  values = c(unadjusted = "#4E79A7", adjusted = "#E15759")
)
chart_line <- chart_data_fill(chart_line,
  values = c(unadjusted = "#4E79A7", adjusted = "#E15759")
)
chart_line <- chart_data_line_width(chart_line,
  values = c(unadjusted = 2, adjusted = 2.5)
)
chart_line <- chart_data_line_style(chart_line,
  values = c(unadjusted = "solid", adjusted = "dashed")
)
chart_line <- chart_labels_text(chart_line,
  values = list(
    unadjusted = fp_text(font.size = 8, color = "#4E79A7"),
    adjusted = fp_text(font.size = 8, color = "#E15759")
  )
)
chart_line <- chart_labels(chart_line,
  title = "US industrial production",
  xlab = "Date", ylab = "Index"
)
chart_line <- chart_ax_x(chart_line, num_fmt = "yyyy")
chart_line <- chart_ax_y(chart_line, num_fmt = "#,##0")
chart_line <- chart_theme(chart_line,
  grid_minor_line_y = fp_border(color = "#EEEEEE", style = "dotted"),
  main_title = fp_text(bold = TRUE, font.size = 18, color = "#333333")
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_line, location = ph_location_fullsize())


# -- slide 3: areachart (percent stacked) -----------------------------------
chart_area <- ms_areachart(
  data = browser_ts, x = "date",
  y = "freq", group = "browser"
)
chart_area <- chart_settings(chart_area, grouping = "percentStacked")
chart_area <- chart_data_fill(chart_area,
  values = c(
    Chrome = "#4285F4", IE = "#0078D7", Firefox = "#FF7139",
    Safari = "#006CFF", Opera = "#FF1B2D", Android = "#A4C639"
  )
)
chart_area <- chart_data_stroke(chart_area,
  values = c(
    Chrome = "#FFFFFF", IE = "#FFFFFF", Firefox = "#FFFFFF",
    Safari = "#FFFFFF", Opera = "#FFFFFF", Android = "#FFFFFF"
  )
)
chart_area <- chart_labels(chart_area,
  title = "Browser market share", ylab = "Share"
)
chart_area <- chart_ax_x(chart_area, num_fmt = "mmm yyyy")
chart_area <- chart_ax_y(chart_area, num_fmt = "0%%")
chart_area <- chart_theme(chart_area,
  grid_major_line_x = FALSE,
  grid_major_line_y = fp_border(color = "#CCCCCC"),
  legend_position = "r"
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_area, location = ph_location_fullsize())


# -- slide 4: scatterchart (smooth markers, data labels) --------------------
scatter_data <- data.frame(
  x = c(1, 2, 3, 5, 8, 1.5, 3, 4.5, 6, 9),
  y = c(2, 4, 5, 8, 13, 3, 7, 6, 10, 15),
  grp = rep(c("Group A", "Group B"), each = 5)
)
chart_scatter <- ms_scatterchart(
  data = scatter_data, x = "x", y = "y", group = "grp"
)
chart_scatter <- chart_settings(chart_scatter, style = "smoothMarker")
chart_scatter <- chart_data_symbol(chart_scatter,
  values = c("Group A" = "circle", "Group B" = "square")
)
chart_scatter <- chart_data_size(chart_scatter,
  values = c("Group A" = 6, "Group B" = 7)
)
chart_scatter <- chart_data_fill(chart_scatter,
  values = c("Group A" = "#59A14F", "Group B" = "#EDC948")
)
chart_scatter <- chart_data_stroke(chart_scatter,
  values = c("Group A" = "#3D7235", "Group B" = "#C4A630")
)
chart_scatter <- chart_data_line_width(chart_scatter,
  values = c("Group A" = 2, "Group B" = 2)
)
chart_scatter <- chart_data_labels(chart_scatter,
  show_val = TRUE, position = "t"
)
chart_scatter <- chart_labels(chart_scatter,
  title = "Scatter with smooth lines", xlab = "X", ylab = "Y"
)
chart_scatter <- chart_ax_x(chart_scatter,
  limit_min = 0, limit_max = 10, major_unit = 2
)
chart_scatter <- chart_ax_y(chart_scatter,
  limit_min = 0, limit_max = 18, major_unit = 3
)
chart_scatter <- chart_theme(chart_scatter,
  axis_ticks_x = fp_border(color = "#666666"),
  axis_ticks_y = fp_border(color = "#666666"),
  grid_major_line_x = fp_border(color = "#EEEEEE"),
  grid_major_line_y = fp_border(color = "#EEEEEE")
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_scatter, location = ph_location_fullsize())


# -- slide 5: bubblechart ---------------------------------------------------
bubble_data <- data.frame(
  x = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
  y = c(50, 40, 60, 30, 70, 45, 55, 35, 65, 25),
  sz = c(5, 10, 15, 8, 20, 7, 12, 9, 18, 6),
  grp = rep(c("Product A", "Product B"), each = 5)
)
chart_bubble <- ms_bubblechart(
  data = bubble_data, x = "x", y = "y",
  size = "sz", group = "grp"
)
chart_bubble <- chart_settings(chart_bubble, bubble3D = TRUE)
chart_bubble <- chart_data_fill(chart_bubble,
  values = c("Product A" = "#76B7B2", "Product B" = "#FF9DA7")
)
chart_bubble <- chart_data_stroke(chart_bubble,
  values = c("Product A" = "#4E8B87", "Product B" = "#CC7D87")
)
chart_bubble <- chart_labels(chart_bubble,
  title = "Bubble chart", xlab = "Revenue", ylab = "Satisfaction"
)
chart_bubble <- chart_ax_x(chart_bubble, num_fmt = "#,##0")
chart_bubble <- chart_ax_y(chart_bubble, num_fmt = "#,##0")
chart_bubble <- chart_theme(chart_bubble,
  plot_border = fp_border(color = "#AAAAAA", width = 1),
  legend_position = "t"
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_bubble, location = ph_location_fullsize())


# -- slide 6: radarchart (filled) -------------------------------------------
radar_data <- data.frame(
  axis = rep(c("Speed", "Power", "Range", "Comfort", "Safety", "Price"), 2),
  value = c(8, 6, 7, 9, 8, 5, 6, 9, 5, 7, 6, 8),
  model = rep(c("Model X", "Model Y"), each = 6)
)
chart_radar <- ms_radarchart(
  data = radar_data, x = "axis", y = "value", group = "model"
)
chart_radar <- chart_settings(chart_radar, style = "filled")
chart_radar <- chart_data_fill(chart_radar,
  values = c("Model X" = "#4E79A780", "Model Y" = "#F28E2B80")
)
chart_radar <- chart_data_stroke(chart_radar,
  values = c("Model X" = "#4E79A7", "Model Y" = "#F28E2B")
)
chart_radar <- chart_labels(chart_radar, title = "Vehicle comparison")
chart_radar <- chart_theme(chart_radar,
  main_title = fp_text(bold = TRUE, font.size = 16, color = "#444444"),
  legend_position = "b"
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_radar, location = ph_location_fullsize())


# -- slide 7: piechart (doughnut) -------------------------------------------
pie_data <- data.frame(
  category = c("Desktop", "Mobile", "Tablet", "Other"),
  value = c(55, 30, 10, 5)
)
chart_pie <- ms_piechart(data = pie_data, x = "category", y = "value")
chart_pie <- chart_settings(chart_pie,
  vary_colors = TRUE, hole_size = 50
)
chart_pie <- chart_data_fill(chart_pie, values = c(
  Desktop = "#4E79A7", Mobile = "#F28E2B",
  Tablet = "#E15759", Other = "#76B7B2"
))
chart_pie <- chart_data_stroke(chart_pie, values = c(
  Desktop = "#FFFFFF", Mobile = "#FFFFFF",
  Tablet = "#FFFFFF", Other = "#FFFFFF"
))
chart_pie <- chart_data_labels(chart_pie,
  show_val = TRUE, show_cat_name = TRUE, show_percent = TRUE,
  separator = "\n"
)
chart_pie <- chart_labels(chart_pie, title = "Device usage (doughnut)")
chart_pie <- chart_theme(chart_pie,
  main_title = fp_text(bold = TRUE, font.size = 18),
  legend_position = "n"
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_pie, location = ph_location_fullsize())


# -- slide 8: stockchart HLC -----------------------------------------------
stock_data <- data.frame(
  date = as.Date("2024-01-01") + 0:9,
  open = c(44, 32, 35, 34, 35, 43, 40, 38, 42, 45),
  high = c(55, 57, 57, 58, 58, 60, 59, 55, 58, 62),
  low = c(11, 12, 13, 11, 25, 20, 18, 15, 22, 28),
  close = c(32, 35, 34, 35, 43, 40, 38, 42, 45, 50)
)
chart_stock_hlc <- ms_stockchart(
  data = stock_data, x = "date",
  high = "high", low = "low", close = "close"
)
chart_stock_hlc <- chart_data_symbol(chart_stock_hlc,
  values = c(high = "none", low = "none", close = "circle")
)
chart_stock_hlc <- chart_data_size(chart_stock_hlc,
  values = c(high = 2, low = 2, close = 5)
)
chart_stock_hlc <- chart_data_fill(chart_stock_hlc,
  values = c(high = "transparent", low = "transparent", close = "#E15759")
)
chart_stock_hlc <- chart_data_stroke(chart_stock_hlc,
  values = c(high = "transparent", low = "transparent", close = "#E15759")
)
chart_stock_hlc <- chart_settings(chart_stock_hlc,
  hi_low_lines = fp_border(color = "#666666", width = 1)
)
chart_stock_hlc <- chart_labels(chart_stock_hlc,
  title = "Stock HLC chart", xlab = "Date", ylab = "Price"
)
chart_stock_hlc <- chart_ax_x(chart_stock_hlc, num_fmt = "dd/mm")
chart_stock_hlc <- chart_ax_y(chart_stock_hlc, num_fmt = "#,##0")
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_stock_hlc, location = ph_location_fullsize())


# -- slide 9: stockchart OHLC (candlestick) --------------------------------
chart_stock_ohlc <- ms_stockchart(
  data = stock_data, x = "date",
  open = "open", high = "high", low = "low", close = "close"
)
chart_stock_ohlc <- chart_data_symbol(chart_stock_ohlc,
  values = c(open = "none", high = "none", low = "none", close = "none")
)
chart_stock_ohlc <- chart_settings(chart_stock_ohlc,
  hi_low_lines = fp_border(color = "#333333", width = 0.75),
  up_bars_fill = "#59A14F",
  up_bars_border = fp_border(color = "#3D7235", width = 0.5),
  down_bars_fill = "#E15759",
  down_bars_border = fp_border(color = "#B33E40", width = 0.5)
)
chart_stock_ohlc <- chart_labels(chart_stock_ohlc,
  title = "Stock OHLC candlestick", xlab = "Date", ylab = "Price"
)
chart_stock_ohlc <- chart_ax_x(chart_stock_ohlc, num_fmt = "dd/mm")
chart_stock_ohlc <- chart_ax_y(chart_stock_ohlc,
  limit_min = 0, limit_max = 70, num_fmt = "#,##0"
)
chart_stock_ohlc <- chart_theme(chart_stock_ohlc,
  grid_major_line_x = FALSE,
  grid_major_line_y = fp_border(color = "#DDDDDD", style = "dashed")
)
doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_stock_ohlc, location = ph_location_fullsize())


# -- slide 10: chart_combine (bar + line, secondary Y, ggplot2 theme) -------
combo_data <- data.frame(
  month = factor(month.abb[1:6], levels = month.abb[1:6]),
  sales = c(120, 150, 170, 160, 200, 220),
  margin = c(12, 14, 13, 15, 18, 20)
)

chart_combo_bar <- ms_barchart(
  data = combo_data, x = "month", y = "sales"
)
chart_combo_bar <- chart_data_fill(chart_combo_bar,
  values = c(sales = "#4E79A7")
)
chart_combo_bar <- chart_data_stroke(chart_combo_bar,
  values = c(sales = "#3B5F87")
)
chart_combo_bar <- chart_labels(chart_combo_bar,
  title = "Sales & margin", xlab = "Month", ylab = "Sales"
)

chart_combo_line <- ms_linechart(
  data = combo_data, x = "month", y = "margin"
)
chart_combo_line <- chart_settings(chart_combo_line, style = "lineMarker")
chart_combo_line <- chart_data_stroke(chart_combo_line,
  values = c(margin = "#E15759")
)
chart_combo_line <- chart_data_fill(chart_combo_line,
  values = c(margin = "#E15759")
)
chart_combo_line <- chart_data_symbol(chart_combo_line,
  values = c(margin = "circle")
)
chart_combo_line <- chart_data_size(chart_combo_line,
  values = c(margin = 5)
)
chart_combo_line <- chart_labels(chart_combo_line, ylab = "Margin (%)")

chart_combo <- ms_chart_combine(
  bars = chart_combo_bar,
  line = chart_combo_line,
  secondary_y = "line"
)
chart_combo <- theme_ggplot2(chart_combo)

doc <- add_slide(doc, layout = "Title and Content")
doc <- ph_with(doc, chart_combo, location = ph_location_fullsize())


# -- write output -----------------------------------------------------------
out <- print(doc, target = tempfile(fileext = ".pptx"))
message("Created: ", out)
browseURL(out)
