# Combined chart object

Combine several chart objects into a single chart with shared axes. Each
chart must be a named argument.

The title and x-axis label are taken from the first chart. The y-axis
label of the first chart on the secondary axis is used as the secondary
y-axis label.

Only one secondary y-axis (right) and one secondary x-axis (top) are
supported.

## Usage

``` r
ms_chart_combine(..., secondary_y = NULL, secondary_x = NULL)
```

## Arguments

- ...:

  named `ms_chart` objects.

- secondary_y:

  character vector of chart names to plot on the secondary (right)
  y-axis.

- secondary_x:

  character vector of chart names to plot on the secondary (top) x-axis.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/dev/reference/chart_settings.md),
[`chart_ax_x()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_x.md),
[`chart_ax_y()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_y.md),
[`chart_data_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_labels.md),
[`chart_theme()`](https://ardata-fr.github.io/mschart/dev/reference/set_theme.md),
[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md),
[`ms_paretochart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_paretochart.md),
[`ms_piechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_piechart.md),
[`ms_radarchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_radarchart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_scatterchart.md),
[`ms_stockchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_stockchart.md),
[`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_sunburstchart.md),
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_treemapchart.md),
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_waterfallchart.md)

## Examples

``` r
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

# example chart 02 -------
# stacked bars + target line on a shared axis (no secondary axis).

browser_data <- data.frame(
  browser = rep(c("Android", "Chrome", "IE", "Firefox", "Opera", "Safari"),
                each = 3),
  serie   = rep(c("v1", "v2", "v3"), times = 6),
  value   = c(80, 90, 77, 150, 140, 131, 60, 70, 62,
              50, 60, 52, 40, 50, 42, 55, 65, 55)
)

target <- data.frame(
  browser = unique(browser_data$browser),
  cible   = 220
)

bars <- ms_barchart(browser_data, x = "browser",
                    y = "value", group = "serie")
bars <- as_bar_stack(bars)

line <- ms_linechart(target, x = "browser", y = "cible")
line <- chart_data_stroke(line, values = "#D62728")
line <- chart_data_line_width(line, values = 2)
line <- chart_data_symbol(line, values = "none")

cb2 <- ms_chart_combine(bars = bars, target = line)

# example chart 03 -------
# two scatter charts on a shared y axis with independent x ranges.
# bottom (primary) chart uses temperature in Celsius;
# top (secondary_x) chart uses pressure in hPa.
# y series must have distinct column names (no collision in the
# embedded sheet) — here humidity_t vs humidity_p.

set.seed(1)
n <- 20
weather <- data.frame(
  temperature_c = runif(n, 5, 30),
  pressure_hpa  = runif(n, 990, 1030),
  humidity_t    = runif(n, 30, 90),
  humidity_p    = runif(n, 30, 90)
)

sc_temp <- ms_scatterchart(
  data = weather, x = "temperature_c", y = "humidity_t"
)
sc_temp <- chart_labels(
  sc_temp, title = "Humidity vs temperature and pressure",
  xlab = "Temperature (°C)", ylab = "Humidity (%)"
)

sc_pres <- ms_scatterchart(
  data = weather, x = "pressure_hpa", y = "humidity_p"
)
sc_pres <- chart_data_symbol(sc_pres, values = "triangle")
sc_pres <- chart_labels(sc_pres, xlab = "Pressure (hPa)")

cb3 <- ms_chart_combine(
  temp = sc_temp, pres = sc_pres,
  secondary_x = "pres"
)
```
