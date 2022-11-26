library(mschart)
library(officer)


# example chart 01 -------

chart_01 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
chart_01 <- chart_settings(
  x = chart_01, dir = "vertical",
  grouping = "clustered", gap_width = 50
)
chart_01 <- chart_ax_x(
  x = chart_01, cross_between = "between",
  major_tick_mark = "out"
)
chart_01 <- chart_ax_y(
  x = chart_01, cross_between = "midCat",
  major_tick_mark = "in"
)


# example chart 02 -------
dat <- data.frame(
  Species = factor(c("setosa", "versicolor", "virginica"),
    levels = c("setosa", "versicolor", "virginica")
  ),
  mean = c(5.006, 5.936, 6.588)
)
chart_02 <- ms_barchart(data = dat, x = "Species", y = "mean")
chart_02 <- chart_settings(x = chart_02, dir = "horizontal")
chart_02 <- chart_theme(x = chart_02, title_x_rot = 270, title_y_rot = 0)



# example chart 03 -------

mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "gray", font.size = 20, bold = TRUE),
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
  gap_width = 150, overlap = 100
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


# example chart 04 -------

dat_groups <-
  data.frame(
    cut = c(
      "Fair", "Fair", "Fair", "Fair", "Fair",
      "Fair", "Fair", "Fair", "Good", "Good", "Good", "Good", "Good",
      "Good", "Good", "Good", "Very Good", "Very Good", "Very Good",
      "Very Good", "Very Good", "Very Good", "Very Good", "Very Good",
      "Premium", "Premium", "Premium", "Premium", "Premium",
      "Premium", "Premium", "Premium", "Ideal", "Ideal", "Ideal", "Ideal",
      "Ideal", "Ideal", "Ideal", "Ideal"
    ),
    clarity = c(
      "I1", "SI2", "SI1", "VS2", "VS1", "VVS2",
      "VVS1", "IF", "I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1",
      "IF", "I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF",
      "I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF", "I1",
      "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF"
    ),
    carat = c(
      1.065, 1.01, 0.98, 0.9, 0.77, 0.7, 0.7,
      0.47, 1.07, 1, 0.79, 0.82, 0.7, 0.505, 0.4, 0.46, 1.145, 1.01,
      0.77, 0.71, 0.7, 0.4, 0.36, 0.495, 1.11, 1.04, 0.9, 0.72, 0.7,
      0.455, 0.4, 0.36, 1.13, 1, 0.71, 0.53, 0.53, 0.44, 0.4, 0.34
    ),
    n = c(
      210L, 466L, 408L, 261L, 170L, 69L, 17L, 9L,
      96L, 1081L, 1560L, 978L, 648L, 286L, 186L, 71L, 84L, 2100L,
      3240L, 2591L, 1775L, 1235L, 789L, 268L, 205L, 2949L, 3575L, 3357L,
      1989L, 870L, 616L, 230L, 146L, 2598L, 4282L, 5071L, 3589L,
      2606L, 2047L, 1212L
    )
  )

dat_groups$label <- sprintf(
  "carat median is %.01f",
  dat_groups$carat
)
dat_groups

text_prop <- fp_text(font.size = 11, color = "gray")

chart_04 <- ms_barchart(
  data = dat_groups, x = "cut",
  labels = "label", y = "n", group = "clarity"
)
chart_04 <- chart_settings(chart_04,
  grouping = "clustered", dir = "horizontal",
  gap_width = 0
)
chart_04 <- chart_data_labels(chart_04, position = "outEnd")
chart_04 <- chart_labels_text(chart_04, text_prop)
chart_04 <- chart_theme(chart_04, title_x_rot = 270, title_y_rot = 0)

# example chart 05 -------

dat_no_group <- data.frame(
  stringsAsFactors = FALSE,
  cut = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
  carat = c(1, 0.82, 0.71, 0.86, 0.54),
  n = c(1610L, 4906L, 12082L, 13791L, 21551L),
  label = c(
    "carat median is 1.0",
    "carat median is 0.8", "carat median is 0.7",
    "carat median is 0.9", "carat median is 0.5"
  )
)
chart_05 <- ms_barchart(
  data = dat_no_group,
  x = "cut", labels = "label", y = "n"
)
chart_05 <- chart_settings(chart_05,
  grouping = "clustered"
)
chart_05 <- chart_data_labels(chart_05, position = "outEnd")
chart_05 <- chart_labels_text(chart_05, text_prop)

# example chart 06 -------
chart_06 <- ms_barchart(
  data = dat_no_group,
  x = "cut", labels = "label", y = "n"
)
chart_06 <- chart_settings(chart_06,
  grouping = "clustered", table = TRUE
)
chart_06 <- chart_data_labels(chart_06, position = "outEnd")
chart_06 <- chart_labels_text(chart_06, text_prop)


