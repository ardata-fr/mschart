# example chart_01 -------

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


# example chart_02 -------

dat <- structure(list(Species = structure(1:3, .Label = c(
  "setosa",
  "versicolor", "virginica"
), class = "factor"), mean = c(
  5.006,
  5.936, 6.588
)), class = "data.frame", .Names = c("Species", "mean"), row.names = c(NA, -3L))

chart_02 <- ms_barchart(data = dat, x = "Species", y = "mean")
chart_02 <- chart_settings(x = chart_02, dir = "horizontal")



# example chart_03 -------

mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange")
)

chart_03 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
chart_03 <- chart_settings(chart_03,
  dir = "horizontal", grouping = "stacked",
  gap_width = 150, overlap = 100
)
chart_03 <- chart_ax_x(chart_03,
  cross_between = "between",
  major_tick_mark = "out", minor_tick_mark = "none"
)
chart_03 <- chart_ax_y(chart_03,
  num_fmt = "0.00", rotation = -90,
  minor_tick_mark = "none"
)
chart_03 <- set_theme(chart_03, mytheme)
chart_03 <- chart_data_labels(chart_03,
  position = "inBase",
  show_val = TRUE, separator = ", ", show_cat_name = TRUE
)






# other examples -------

dat_groups <- structure(list(
  cut = structure(c(
    1L, 1L, 1L, 1L, 1L, 1L, 1L,
    1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
    5L
  ),
  .Label = c("Fair", "Good", "Very Good", "Premium", "Ideal"),
  class = c("ordered", "factor")
  ), clarity = structure(c(
    1L,
    2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L,
    2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L,
    2L, 3L, 4L, 5L, 6L, 7L, 8L
  ), .Label = c(
    "I1", "SI2", "SI1", "VS2",
    "VS1", "VVS2", "VVS1", "IF"
  ), class = c("ordered", "factor")),
  carat = c(
    1.065, 1.01, 0.98, 0.9, 0.77, 0.7, 0.7, 0.47, 1.07,
    1, 0.79, 0.82, 0.7, 0.505, 0.4, 0.46, 1.145, 1.01, 0.77,
    0.71, 0.7, 0.4, 0.36, 0.495, 1.11, 1.04, 0.9, 0.72, 0.7,
    0.455, 0.4, 0.36, 1.13, 1, 0.71, 0.53, 0.53, 0.44, 0.4, 0.34
  ), n = c(
    210L, 466L, 408L, 261L, 170L, 69L, 17L, 9L, 96L,
    1081L, 1560L, 978L, 648L, 286L, 186L, 71L, 84L, 2100L, 3240L,
    2591L, 1775L, 1235L, 789L, 268L, 205L, 2949L, 3575L, 3357L,
    1989L, 870L, 616L, 230L, 146L, 2598L, 4282L, 5071L, 3589L,
    2606L, 2047L, 1212L
  )
), row.names = c(NA, -40L), class = "data.frame")
dat_groups$label <- sprintf(
  "carat median is `%.01.f`",
  dat_groups$carat
)
dat_groups

text_prop <- fp_text(font.size = 11, color = "gray")

## an example with group and label -----
chart_04 <- ms_barchart(
  data = dat_groups, x = "cut",
  labels = "label", y = "n", group = "clarity"
)
chart_04 <- chart_settings(chart_04,
  grouping = "clustered",
  dir = "horizontal", gap_width = 0
)
chart_04 <- chart_data_labels(chart_04, position = "outEnd")
chart_04 <- chart_labels_text(chart_04, text_prop)

## an example with group and no label -----
chart_05 <- ms_barchart(
  data = dat_groups,
  x = "cut", y = "n", group = "clarity"
)
chart_05 <- chart_settings(chart_05,
  grouping = "clustered", dir = "horizontal"
)


dat_no_group <- structure(list(
  cut = structure(1:5,
    .Label = c(
      "Fair", "Good", "Very Good", "Premium", "Ideal"
    ),
    class = c("ordered", "factor")
  ), carat = c(1, 0.82, 0.71, 0.86, 0.54),
  n = c(1610L, 4906L, 12082L, 13791L, 21551L)
), row.names = c(NA, -5L), class = "data.frame")
dat_no_group$label <- sprintf(
  "carat median is `%.01.f`",
  dat_no_group$carat
)
dat_no_group

## an example with no group and a label -----
chart_06 <- ms_barchart(
  data = dat_no_group,
  x = "cut", labels = "label", y = "n"
)
chart_06 <- chart_settings(chart_06,
  grouping = "clustered", dir = "horizontal"
)
chart_06 <- chart_data_labels(chart_06, position = "outEnd")
chart_06 <- chart_labels_text(chart_06, text_prop)

## an example with no group and no label -----
chart_07 <- ms_barchart(
  data = dat_no_group,
  x = "cut", y = "n"
)
chart_07 <- chart_settings(chart_07,
  grouping = "clustered", dir = "horizontal"
)
