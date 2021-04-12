# example chart_01 -------
chart_01 <- ms_scatterchart(data = mtcars, x = "disp",
                         y = "drat")
chart_01 <- chart_ax_x(chart_01, cross_between = "midCat")
chart_01 <- chart_settings(chart_01, scatterstyle = "marker")


# example chart_02 -------
chart_02 <- ms_scatterchart(data = iris, x = "Sepal.Length", y = "Petal.Length", group = "Species")
chart_02 <- chart_settings(chart_02, scatterstyle = "marker")


