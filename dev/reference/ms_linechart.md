# Linechart object

Creation of a linechart object that can be inserted in a 'Microsoft'
document.

In a line chart, category data is distributed evenly along the
horizontal axis, and all value data is distributed evenly along the
vertical axis. Line charts can show continuous data over time on an
evenly scaled axis, so they're ideal for showing trends in data at equal
intervals, like months and quarters.

## Usage

``` r
ms_linechart(data, x, y, group = NULL, labels = NULL, asis = FALSE)
```

## Arguments

- data:

  a data.frame

- x:

  column name for x values.

- y:

  column name for y values.

- group:

  grouping column name used to split data into series. Optional.

- labels:

  column names of columns to be used as custom data labels displayed
  next to data points (not axis labels). Optional. If more than one name
  is provided, only the first one will be used as a label, but all
  labels (transposed if a group is used) will be available in the Excel
  file associated with the chart.

- asis:

  logical parameter defaulting to FALSE. When FALSE, the data is
  reshaped internally so that each series becomes a separate column.
  When TRUE, the data is used as-is and must already have one column for
  categories and one column per series, and `y` accepts a vector of
  series column names. `asis` describes the *input shape* read by the
  constructor. Not to be confused with the `write_data` argument of
  [`sheet_add_drawing.ms_chart()`](https://ardata-fr.github.io/mschart/dev/reference/sheet_add_drawing.ms_chart.md),
  which controls whether `mschart` writes the chart's data into an Excel
  sheet at embed time. The two are independent.

## Value

An `ms_chart` object.

## Illustrations

![](figures/fig_ms_linechart_1.png)

![](figures/fig_ms_linechart_2.png)

![](figures/fig_ms_linechart_3.png)

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
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md),
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
library(officer)
# example chart_01 -------
chart_01 <- ms_linechart(
  data = us_indus_prod,
  x = "date", y = "value",
  group = "type"
)

chart_01 <- chart_ax_x(
  x = chart_01, num_fmt = "[$-fr-FR]mmm yyyy",
  limit_min = min(us_indus_prod$date), limit_max = as.Date("1992-01-01")
)

chart_01 <- chart_data_stroke(
  x = chart_01,
  values = c(adjusted = "red", unadjusted = "gray")
)

chart_01 <- chart_data_line_width(
  x = chart_01,
  values = c(adjusted = 2, unadjusted = 5)
)

chart_01 <- chart_theme(chart_01,
  grid_major_line_x = fp_border(width = 0),
  grid_minor_line_x = fp_border(width = 0)
)

# example chart_02 -------
data <- data.frame(
  supp = factor(rep(c("OJ", "VC"), each = 3), levels = c("OJ", "VC")),
  dose = factor(rep(c("low", "medium", "high"), 2), levels = c("low", "medium", "high")),
  length = c(13.23, 22.7, 24.06, 7.98, 16.77, 26.14),
  label = LETTERS[1:6],
  stringsAsFactors = FALSE
)

chart_02 <- ms_linechart(
  data = data, x = "dose", y = "length",
  group = "supp", labels = "label"
)
chart_02 <- chart_ax_y(
  x = chart_02, cross_between = "between",
  limit_min = 5, limit_max = 30,
  num_fmt = "General"
)
chart_02 <- chart_data_labels(
  x = chart_02, position = "l"
)

# example chart 03 -------
chart_03 <- ms_linechart(
  data = data, x = "dose", y = "length",
  group = "supp", labels = "label"
)
chart_03 <- chart_ax_y(
  x = chart_03, cross_between = "between",
  limit_min = 5, limit_max = 30,
  num_fmt = "General"
)
chart_03 <- chart_data_labels(
  x = chart_03, position = "l"
)

chart_03 <- chart_settings(
  x = chart_03, table = TRUE
)

chart_03 <- chart_table(chart_03,
  horizontal = TRUE, vertical = FALSE,
  outline = TRUE, show_keys = FALSE
)
```
