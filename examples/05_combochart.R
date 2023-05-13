library(mschart)


# example chart 01 -------

dat <- longley
dat$Year <- as.Date(paste0(dat$Year, "-01-01"))
dat$`GNP Deflator` <- dat$GNP.deflator

empl <- dat |>
  ms_barchart(x = "Year", y = c("Employed", "Unemployed"), asis = TRUE) |>
  chart_labels(title = "Longley's Economic Regression Data",
               xlab = "1947 to 1962", ylab = "Employment") |>
  chart_ax_x(num_fmt = "yyyy") |>
  as_bar_stack(dir = "vertical")

gdp <- dat |>
  ms_linechart(x = "Year", y = "GNP Deflator", asis = TRUE) |>
  chart_labels(ylab = "GNP implicit price deflator (1954 = 100)") |>
  chart_ax_y(second_axis = TRUE)

cb <- ms_combochart(empl, gdp)
