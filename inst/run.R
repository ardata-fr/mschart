library(data.table)
library(officer)
library(magrittr)
library(mschart)

dat <- data.frame(labels = letters[1:5], serie1 = 1:5, serie2 = 6:10, serie3 = 11:15, stringsAsFactors = FALSE) %>%
  as.data.table() %>% melt(id="labels", measure =c("serie1", "serie2", "serie3"),
       variable.name = "serie", value.name = "value", variable.factor = FALSE)
dat$serie <- factor( dat$serie, levels = c("serie1", "serie3", "serie2") )

x_ax <- axis_options(axis_position = "b" )
y_ax <- axis_options(axis_position = "l", num_fmt = "#,##0.00")

x1 <- ms_barchart(data = dat, x = "labels", y = "value", group = "serie") %>%
  set_bar_options( barchart_options(dir="col", grouping="clustered") ) %>%
  set_x_axis(options = x_ax) %>%
  set_y_axis(options = y_ax)

x_ax <- axis_options(axis_position = "b" )
y_ax <- axis_options(axis_position = "l", num_fmt = "0.00")

mytheme <- mschart_theme(
  axis.title.x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis.title.y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid.minor.line = fp_border(width = 0, color = "transparent")
  )

x2 <- ms_barchart(data = dat, x = "labels", y = "value") %>%
  set_bar_options( barchart_options(dir="col", grouping="clustered") ) %>%
  set_x_axis(options = x_ax) %>%
  set_y_axis(options = y_ax) %>%
  set_labels(xlab = "coucou") %>% set_mschart_theme(value = mytheme ) %>%
  set_labels(ylab = "you you") %>%
  set_data_label(data_labels_options(position = "outEnd", show_val = TRUE))

x3_bar_opt <- barchart_options(dir="bar", grouping="stacked", gap_width = 150, overlap = 100)
x3 <- ms_barchart(data = dat, x = "labels", y = "value", group = "serie") %>%
  set_bar_options( x3_bar_opt ) %>%
  set_x_axis(options = x_ax) %>%
  set_y_axis(options = y_ax) %>% set_mschart_theme(mytheme) %>%
  set_data_label(data_labels_options(position = "inBase", show_val = TRUE, separator = ", ", show_cat_name = TRUE))



doc <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x1)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x2)
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with_chart(doc, value = x3)

target_ <- tempfile(fileext = ".pptx")
# target_ <- "run.pptx"

print(doc, target = target_) %>% browseURL()

# unlink("bbbbb", recursive = TRUE, force = TRUE)
# unpack_folder("run.pptx", "bbbbb")
# unpack_folder("run.pptx", "aaaaa")



