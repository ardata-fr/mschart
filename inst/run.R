library(officer)
library(magrittr)
library(mschart)


mytheme <- mschart_theme(main_title = fp_text(color = "orange", font.size = 40, bold = TRUE),
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = .25, color = "orange", style = "dashed"),
  grid_major_line_x = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange"), legend_position = "tr" )


x4 <- ms_linechart(data = iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>%
  chart_ax_y(num_fmt = "0.00", rotation = -90) %>%
  set_theme(mytheme)


x5 <- ms_linechart(data = browser_ts, x = "date", y = "freq", group = "browser") %>%
  chart_ax_y(cross_between = "between", num_fmt = "General") %>%
  chart_ax_x(cross_between = "midCat", num_fmt = "m/d/yy") %>%
  set_theme(mytheme)

x6 <- ms_linechart(data = browser_ts, x = "date", y = "freq", group = "browser") %>%
  chart_ax_x(cross_between = "midCat", num_fmt = "m/d/yy") %>%
  chart_settings(grouping = "percentStacked") %>%
  chart_labels(title = "coco") %>% set_theme(mytheme)

mytheme <- mschart_theme(
  axis_title = fp_text(color = "red", font.size = 24, bold = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = .4, color = "gray"), legend_position = "t"  )


my_bc <- ms_barchart(data = browser_data, x = "browser",
                     y = "value", group = "serie")
my_bc <- chart_settings( my_bc, dir="bar", grouping="stacked",
                         gap_width = 150, overlap = 100 )
my_bc <- set_theme(my_bc, mytheme)



my_bc_2 <- ms_barchart(data = browser_data, x = "browser",
                       y = "value", group = "serie") %>%
  chart_data_fill(values = c(serie1 = "red") ) %>%
  chart_data_stroke(values = c(serie1 = "transparent") )

my_bc_2 <- chart_theme(my_bc_2, grid_major_line_x = fp_border(width = .5, color = "red") ,
                        grid_major_line_y = fp_border(width = .5, color = "cyan") )


doc <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = my_bc)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = my_bc_2)

target_ <- tempfile(fileext = ".pptx")
print(doc, target = target_) %>% browseURL()

