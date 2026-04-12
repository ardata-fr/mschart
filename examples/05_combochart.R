# example chart 01 -------

dat <- longley
dat$Year <- as.Date(paste0(dat$Year, "-01-01"))

dat_empl <- data.frame(
  Year = rep(dat$Year, 2),
  value = c(dat$Employed, dat$Unemployed),
  serie = rep(c("Employed", "Unemployed"), each = nrow(dat)),
  stringsAsFactors = FALSE
)

empl <- ms_barchart(
  data = dat_empl, x = "Year",
  y = "value", group = "serie"
)
empl <- chart_labels(
  x = empl, title = "Longley's Economic Regression Data",
  xlab = "1947 to 1962", ylab = "Employment"
)
empl <- chart_ax_x(x = empl, num_fmt = "yyyy")
empl <- as_bar_stack(x = empl, dir = "vertical")

gdp <- ms_linechart(data = dat, x = "Year", y = "GNP.deflator")
gdp <- chart_labels(x = gdp, ylab = "GNP implicit price deflator (1954 = 100)")

cb <- ms_chart_combine(empl = empl, gdp = gdp, secondary_y = "gdp")
