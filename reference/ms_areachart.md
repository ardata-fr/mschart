# Areachart object

Creation of an areachart object that can be inserted in a 'Microsoft'
document.

Area charts can be used to plot change over time and draw attention to
the total value across a trend. By showing the sum of the plotted
values, an area chart also shows the relationship of parts to a whole.

## Usage

``` r
ms_areachart(data, x, y, group = NULL, labels = NULL, asis = FALSE)
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
  [`sheet_add_drawing.ms_chart()`](https://ardata-fr.github.io/mschart/reference/sheet_add_drawing.ms_chart.md),
  which controls whether `mschart` writes the chart's data into an Excel
  sheet at embed time. The two are independent.

## Value

An `ms_chart` object.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md),
[`chart_ax_x()`](https://ardata-fr.github.io/mschart/reference/chart_ax_x.md),
[`chart_ax_y()`](https://ardata-fr.github.io/mschart/reference/chart_ax_y.md),
[`chart_data_labels()`](https://ardata-fr.github.io/mschart/reference/chart_data_labels.md),
[`chart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.md),
[`chart_labels()`](https://ardata-fr.github.io/mschart/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_barchart()`](https://ardata-fr.github.io/mschart/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/reference/ms_histogramchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/reference/ms_linechart.md),
[`ms_paretochart()`](https://ardata-fr.github.io/mschart/reference/ms_paretochart.md),
[`ms_piechart()`](https://ardata-fr.github.io/mschart/reference/ms_piechart.md),
[`ms_radarchart()`](https://ardata-fr.github.io/mschart/reference/ms_radarchart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/reference/ms_scatterchart.md),
[`ms_stockchart()`](https://ardata-fr.github.io/mschart/reference/ms_stockchart.md),
[`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/reference/ms_sunburstchart.md),
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/reference/ms_treemapchart.md),
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/reference/ms_waterfallchart.md)

## Examples

``` r
library(officer)
mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange")
)

# example ac_01 -------
ac_01 <- ms_areachart(
  data = browser_ts, x = "date",
  y = "freq", group = "browser"
)
ac_01 <- chart_ax_y(ac_01, cross_between = "between", num_fmt = "General")
ac_01 <- chart_ax_x(ac_01, cross_between = "midCat", num_fmt = "m/d/yy")
ac_01 <- set_theme(ac_01, mytheme)


# example ac_02 -------
ac_02 <- chart_settings(ac_01, grouping = "percentStacked")

# example ac_03 -------
ac_03 <- chart_settings(ac_01, grouping = "percentStacked", table = TRUE)
ac_03 <- chart_table(
  ac_03,
  horizontal = FALSE, vertical = FALSE,
  outline = FALSE, show_keys = TRUE)
```
