# Modify axis and plot labels

Add labels to a chart, labels can be specified for x axis, y axis and
plot.

## Usage

``` r
chart_labels(x, title = NULL, xlab = NULL, ylab = NULL)
```

## Arguments

- x:

  an `ms_chart` object.

- title:

  title of the chart (displayed above the plot area). Use NULL to remove
  it.

- xlab:

  label for the x axis. Use NULL to remove it.

- ylab:

  label for the y axis. Use NULL to remove it.

## Value

An `ms_chart` object.

## See also

[`chart_data_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_labels.md),
[`chart_ax_x()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_x.md),
[`chart_ax_y()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_y.md)

## Examples

``` r
mylc <- ms_linechart(
  data = browser_ts, x = "date", y = "freq",
  group = "browser"
)
mylc <- chart_labels(mylc,
  title = "my title", xlab = "my x label",
  ylab = "my y label"
)
```
