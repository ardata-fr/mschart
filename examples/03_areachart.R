mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange") )

# example ac_01 -------
ac_01 <- ms_areachart(data = iris, x = "Sepal.Length",
                      y = "Sepal.Width", group = "Species")
ac_01 <- chart_ax_y(ac_01, num_fmt = "0.00", rotation = -90)
ac_01 <- set_theme(ac_01, mytheme)


# example ac_02 -------
ac_02 <- ms_areachart(data = browser_ts, x = "date",
                      y = "freq", group = "browser")
ac_02 <- chart_ax_y(ac_02, cross_between = "between", num_fmt = "General")
ac_02 <- chart_ax_x(ac_02, cross_between = "midCat", num_fmt = "m/d/yy")
ac_02 <- set_theme(ac_02, mytheme)


# example ac_03 -------
ac_03 <- ms_areachart(data = browser_ts, x = "date",
                      y = "freq", group = "browser")
ac_03 <- chart_ax_x(ac_03, cross_between = "midCat", num_fmt = "m/d/yy")
ac_03 <- chart_settings(ac_03, grouping = "percentStacked")

