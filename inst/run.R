library(data.table)
library(officer)
library(magrittr)
library(mschart)

# dat <- data.frame(labels = letters[1:5], serie1 = 1:5, serie2 = 6:10, serie3 = 11:15, stringsAsFactors = FALSE) %>%
#   as.data.table() %>% melt(id="labels", measure =c("serie1", "serie2", "serie3"),
#        variable.name = "serie", value.name = "value", variable.factor = FALSE)
# dat$serie <- factor( dat$serie, levels = c("serie1", "serie3", "serie2") )
#
x_ax <- axis_options(axis_position = "b", cross_between = "midCat" )
y_ax <- axis_options(axis_position = "l", cross_between = "midCat")
#
# x1 <- ms_barchart(data = dat, x = "labels", y = "value", group = "serie") %>%
#   set_bar_options( barchart_options(dir="col", grouping="clustered", gap_width = 50) ) %>%
#   set_x_axis(options = x_ax) %>%
#   set_y_axis(options = y_ax)
#
# x_ax <- axis_options(axis_position = "b" )
# y_ax <- axis_options(axis_position = "l", num_fmt = "0.00", rotation = -90 )
#
mytheme <- mschart_theme(
  axis.title.x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis.title.y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid.minor.line = fp_border(width = 0, color = "transparent")
  )
#
# x2 <- ms_barchart(data = dat, x = "labels", y = "value") %>%
#   set_bar_options( barchart_options(dir="col", grouping="clustered") ) %>%
#   set_x_axis(options = x_ax) %>%
#   set_y_axis(options = y_ax) %>%
#   set_labels(xlab = "coucou") %>%
#   set_mschart_theme(value = mytheme ) %>%
#   set_labels(ylab = "you you") %>%
#   set_data_label(data_labels_options(position = "outEnd", show_val = TRUE))
#
# x3_bar_opt <- barchart_options(dir="bar", grouping="stacked", gap_width = 150, overlap = 100)
# x3 <- ms_barchart(data = dat, x = "labels", y = "value", group = "serie") %>%
#   set_bar_options( x3_bar_opt ) %>%
#   set_x_axis(options = x_ax) %>%
#   set_y_axis(options = y_ax) %>%
#   set_mschart_theme(mytheme) %>%
#   set_data_label(opts = data_labels_options(
#     position = "inBase", show_val = TRUE, separator = ", ", show_cat_name = TRUE) )

x4 <- ms_scatter(data = iris[order(iris$Sepal.Length),], x = "Sepal.Length", y = "Sepal.Width") %>%
  set_x_axis(options = x_ax) %>% #set_scatter_options(scatter_options())
  set_y_axis(options = y_ax) %>%
  set_mschart_theme(mytheme)

x_ax_date <- axis_options(axis_position = "b", cross_between = "midCat", num_fmt = "mm-dd-yy" )

x5 <- ms_scatter(data = df[order(df$browser, df$date),], x = "date", y = "percent", group = "browser") %>%
  set_x_axis(options = x_ax_date) %>% #set_scatter_options(scatter_options())
  set_y_axis(options = y_ax) %>%
  set_mschart_theme(mytheme)



doc <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, value = x1)
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, value = x2)
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
# doc <- ph_with_chart(doc, value = x3)
# doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x4)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x5)

target_ <- tempfile(fileext = ".pptx")
# target_ <- "run.pptx"

print(doc, target = target_) %>% browseURL()

# unlink("bbbbb", recursive = TRUE, force = TRUE)
# unpack_folder("run.pptx", "manual")
# unpack_folder("run.pptx", "auto")



