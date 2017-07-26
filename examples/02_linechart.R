mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange") )

# example lc_01 -------
lc_01 <- ms_linechart(data = iris, x = "Sepal.Length",
                      y = "Sepal.Width", group = "Species")
lc_01 <- chart_ax_y(lc_01, num_fmt = "0.00", rotation = -90)
lc_01 <- set_theme(lc_01, mytheme)


# example lc_02 -------
lc_02 <- ms_linechart(data = browser_ts, x = "date",
                      y = "freq", group = "browser")
lc_02 <- chart_ax_y(lc_02, cross_between = "between", num_fmt = "General")
lc_02 <- chart_ax_x(lc_02, cross_between = "midCat", num_fmt = "m/d/yy")
lc_02 <- set_theme(lc_02, mytheme)


# example lc_03 -------
lc_03 <- ms_linechart(data = browser_ts, x = "date",
                      y = "freq", group = "browser")
lc_03 <- chart_ax_x(lc_03, cross_between = "midCat", num_fmt = "m/d/yy")
lc_03 <- chart_settings(lc_03, grouping = "percentStacked")

