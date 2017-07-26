# example my_barchart_01 -------

my_barchart_01 <- ms_barchart(data = browser_data, x = "browser",
                           y = "value", group = "serie")
my_barchart_01 <- chart_settings( x = my_barchart_01, dir="col",
                               grouping="clustered", gap_width = 50 )
my_barchart_01 <- chart_ax_x( x= my_barchart_01, cross_between = 'between',
                           major_tick_mark="out")
my_barchart_01 <- chart_ax_y( x= my_barchart_01, cross_between = "midCat",
                           major_tick_mark="in")


# example my_barchart_02 -------

dat <- structure(list(Species = structure(1:3, .Label = c("setosa",
  "versicolor", "virginica"), class = "factor"), mean = c(5.006,
  5.936, 6.588)), class = "data.frame", .Names = c("Species", "mean"
  ), row.names = c(NA, -3L))

my_barchart_02 <- ms_barchart(data = dat, x = "Species", y = "mean")
my_barchart_02 <- chart_settings( x = my_barchart_02, dir="bar" )



# example my_barchart_03 -------

mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange") )

my_barchart_03 <- ms_barchart(data = browser_data, x = "browser",
                              y = "value", group = "serie")
my_barchart_03 <- chart_settings( my_barchart_03, dir="bar", grouping="stacked",
                                  gap_width = 150, overlap = 100 )
my_barchart_03 <- chart_ax_x(my_barchart_03, cross_between = 'between',
                             major_tick_mark="out", minor_tick_mark = "none")
my_barchart_03 <- chart_ax_y(my_barchart_03, num_fmt = "0.00", rotation = -90,
                             minor_tick_mark = "none")
my_barchart_03 <- set_theme(my_barchart_03, mytheme)
my_barchart_03 <- chart_data_labels(my_barchart_03, position = "inBase",
                                    show_val = TRUE, separator = ", ", show_cat_name = TRUE)


