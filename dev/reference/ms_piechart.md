# Piechart object

Creation of a piechart object that can be inserted in a 'Microsoft'
document.

Pie charts show the proportion of each category as a slice of a circle.
Doughnut charts are similar but have a hole in the centre. Use
`chart_settings(x, hole_size = ...)` to control the hole size: 0
produces a pie chart, values above 0 produce a doughnut chart.

Data must be pre-aggregated: one row per slice, no grouping column.

## Usage

``` r
ms_piechart(data, x, y, labels = NULL)
```

## Arguments

- data:

  a data.frame

- x:

  column name for categories (slices).

- y:

  column name for values (slice sizes).

- labels:

  column names of columns to be used as custom data labels displayed
  next to data points (not axis labels). Optional. If more than one name
  is provided, only the first one will be used as a label, but all
  labels (transposed if a group is used) will be available in the Excel
  file associated with the chart.

## Value

An `ms_chart` object.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/dev/reference/chart_settings.md),
[`chart_data_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_labels.md),
[`chart_theme()`](https://ardata-fr.github.io/mschart/dev/reference/set_theme.md),
[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md),
[`ms_paretochart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_paretochart.md),
[`ms_radarchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_radarchart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_scatterchart.md),
[`ms_stockchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_stockchart.md),
[`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_sunburstchart.md),
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_treemapchart.md),
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_waterfallchart.md)

## Examples

``` r
library(officer)
library(mschart)

dat <- data.frame(
  browser = c("Chrome", "Firefox", "Safari", "Edge", "Other"),
  value = c(64, 12, 8, 5, 11)
)

# Pie chart
pie <- ms_piechart(data = dat, x = "browser", y = "value")
pie <- chart_labels(pie, title = "Browser share")

# Doughnut chart
donut <- ms_piechart(data = dat, x = "browser", y = "value")
donut <- chart_settings(donut, hole_size = 50)
donut <- chart_labels(donut, title = "Browser share (donut)")
```
