# example chart 01 -------

dat <- longley
dat$Year <- as.Date(paste0(dat$Year, "-01-01"))
dat$`GNP Deflator` <- dat$GNP.deflator

empl <- ms_barchart(data = dat, x = "Year", y = c("Employed", "Unemployed"), asis = TRUE)
empl <- chart_labels(x = empl, title = "Longley's Economic Regression Data",
                     xlab = "1947 to 1962", ylab = "Employment")
empl <- chart_ax_x(x = empl, num_fmt = "yyyy")
empl <- as_bar_stack(x = empl, dir = "vertical")

gdp <- ms_linechart(data = dat, x = "Year", y = "GNP Deflator", asis = TRUE)
gdp <- chart_labels(x = gdp, ylab = "GNP implicit price deflator (1954 = 100)")
gdp <- chart_ax_y(x = gdp, second_axis = TRUE)

cb <- ms_combochart(empl, gdp)
