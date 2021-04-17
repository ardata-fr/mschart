library(mschart)
library(officer)
library(xml2)

source("utils/unpack_chart.R")

# example areachart -------
chart_01 <- ms_areachart(data = browser_ts, x = "date", y = "freq", group = "browser")
chart_01 <- chart_ax_x(chart_01, num_fmt = "m/d/yy", rotation = -90)
chart_01 <- chart_data_labels(chart_01, show_val = TRUE)
chart_01 <- chart_labels_text( chart_01, values = fp_text(font.size = 7, color = "red") )

path <- unpack_chart(chart_01)

chart <- read_xml(attr(path, "chart_xml"))
dLblPos <- xml_find_all(chart, "//c:ser/c:dLbls/c:dLblPos")

# test that no dLblPos is found in the series
expect_true(length(dLblPos) < 1)

