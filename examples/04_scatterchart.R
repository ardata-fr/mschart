# example sc_01 -------
sc_01 <- ms_scatterchart(data = mtcars, x = "disp",
                         y = "drat")
sc_01 <- chart_ax_x(sc_01, cross_between = "midCat")
sc_01 <- chart_settings(sc_01, scatterstyle = "marker")


