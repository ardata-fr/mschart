library(officer)
library(magrittr)
library(mschart)

x1 <- ms_barchart(data = browser_data, x = "browser", y = "value", group = "serie") %>%
  chart_settings( dir="col", grouping="clustered", gap_width = 50 ) %>%
  set_x_axis(cross_between = 'between', major_tick_mark="out") %>%
  set_y_axis(cross_between = "midCat", major_tick_mark="in")


mytheme <- chart_theme(
  axis.title.x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis.title.y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid.major.line.y = fp_border(width = 1, color = "orange"),
  axis.ticks.y = fp_border(width = 1, color = "orange")
  )

x2 <- ms_barchart(data = browser_data, x = "browser", y = "value") %>%
  chart_settings( dir="col", grouping="clustered" ) %>%
  set_labels(ylab = "you you") %>%
  set_data_label(data_labels_options(position = "outEnd", show_val = TRUE))

x3 <- ms_barchart(data = browser_data, x = "browser", y = "value", group = "serie") %>%
  chart_settings( dir="bar", grouping="stacked", gap_width = 150, overlap = 100 )  %>%
  set_x_axis(cross_between = 'between', major_tick_mark="out", minor_tick_mark = "none") %>%
  set_y_axis(num_fmt = "0.00", rotation = -90, minor_tick_mark = "none") %>%
  set_mschart_theme(mytheme) %>%
  set_data_label(opts = data_labels_options(
    position = "inBase", show_val = TRUE, separator = ", ", show_cat_name = TRUE) )

x4 <- ms_linechart(data = iris[order(iris$Sepal.Length),], x = "Sepal.Length", y = "Sepal.Width", group = "Species") %>%
  set_y_axis(num_fmt = "0.00", rotation = -90) %>%
  set_mschart_theme(mytheme)


x5 <- ms_linechart(data = browser_ts, x = "date", y = "freq", group = "browser") %>%
  set_y_axis(cross_between = "between", num_fmt = "General") %>%
  set_x_axis(cross_between = "midCat", num_fmt = "m/d/yy") %>%
  set_mschart_theme(mytheme)

x6 <- ms_linechart(data = browser_ts, x = "date", y = "freq", group = "browser") %>%
  set_x_axis(cross_between = "midCat", num_fmt = "m/d/yy") %>%
  chart_settings(grouping = "percentStacked")



doc <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x1)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x2)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x3)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x4)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x5)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x6)

target_ <- tempfile(fileext = ".pptx")
print(doc, target = target_) %>% browseURL()

# unlink("bbbbbbb", recursive = TRUE, force = TRUE)
# unpack_folder(target_, "bbbbbbb")
# rm(target_)
