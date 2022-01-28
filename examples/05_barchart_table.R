library(officer)
library(dplyr)
debug(mschart::format.ms_chart)
chart_01 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
debug(mschart::chart_settings)
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

read_docx() %>%
  body_add_chart(chart = chart_01, style = "centered") %>%
  print(target = "example_word_chart.docx")
