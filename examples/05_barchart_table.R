library(officer)
library(dplyr)
# debug(mschart::format.ms_chart)
chart_01 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
# debug(mschart::chart_settings)
chart_01 <- chart_settings(
  x = chart_01, dir = "vertical",
  grouping = "clustered", gap_width = 50,
  table = TRUE
)

chart_01 <- chart_ax_x(
  x = chart_01, cross_between = "between",
  major_tick_mark = "out"
)

chart_01 <- chart_ax_y(
  x = chart_01, cross_between = "midCat",
  major_tick_mark = "in"
)
# debug(body_add_chart)
read_docx() %>%
  body_add_chart(chart = chart_01, style = "centered") %>%
  print(target = "example_word_chart.docx")

# example2 ----------------------------------------------------------------
mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "gray", font.size = 20, bold = TRUE),
  table_text = fp_text(color = "red", font.size = 15, bold = FALSE),
  axis_title_y = fp_text(color = "gray", font.size = 20, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "wheat"),
  axis_ticks_y = fp_border(width = 1, color = "gray")
)

chart_03 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
chart_03 <- chart_settings(chart_03,
  grouping = "stacked",
  gap_width = 150, overlap = 100,
  table = T
)
chart_03 <- chart_ax_x(chart_03,
  cross_between = "between",
  major_tick_mark = "out", minor_tick_mark = "none"
)
chart_03 <- chart_ax_y(chart_03,
  num_fmt = "0.00",
  minor_tick_mark = "none"
)
chart_03 <- set_theme(chart_03, mytheme)
chart_03 <- chart_labels(x = chart_03, title = "Things in percent")
chart_03 <- chart_data_labels(chart_03,
  position = "ctr",
  show_val = TRUE
)
chart_03 <- chart_labels_text(chart_03, fp_text(color = "white", bold = TRUE, font.size = 9))

read_docx() %>%
  body_add_chart(chart = chart_03, style = "centered") %>%
  print(target = "example_word_chart.docx")

# ejemplo 3 ---------------------------------------------------------------
mytheme <- mschart_theme(
  axis_title_x = fp_text(font.size = 20, bold = TRUE),
  table_text = fp_text(color = "green", font.size = 15, bold = FALSE),
  axis_title_y = fp_text(font.size = 20, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "wheat"),
  axis_ticks_y = fp_border(width = 1, color = "gray")
)

chart_04 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
chart_04 <- chart_settings(chart_04,
  grouping = "stacked",
  gap_width = 150, overlap = 100,
  table = T
)
chart_04 <- chart_ax_x(chart_04,
  cross_between = "between",
  major_tick_mark = "out", minor_tick_mark = "none"
)
chart_04 <- chart_ax_y(chart_04,
  num_fmt = "0.00",
  minor_tick_mark = "none"
)

chart_04 <- chart_ax_y(chart_04,
  num_fmt = "0.00",
  minor_tick_mark = "none"
)
chart_04 <- set_theme(chart_04, mytheme)
chart_04 <- chart_labels(x = chart_04, title = "Things in percent")
chart_04 <- chart_data_labels(chart_04,
  position = "ctr",
  show_val = TRUE
)
chart_04 <- chart_labels_text(chart_04, fp_text(color = "white", bold = TRUE, font.size = 9))

chart_04 <- chart_table(chart_04, horizontal = T, vertical = F, outline = T, show_keys = F)

read_docx() %>%
  body_add_chart(chart = chart_04, style = "centered") %>%
  print(target = "example_word_chart.docx")


