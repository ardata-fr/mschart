# Smooth series

Specify whether lines should be smoothed, per series. This feature only
applies to
[`ms_linechart()`](https://ardata-fr.github.io/mschart/reference/ms_linechart.md).

## Usage

``` r
chart_data_smooth(x, values)
```

## Arguments

- x:

  an `ms_chart` object.

- values:

  `integer(num of series)`: a set of smooth values to map data values
  to. It is a named vector, the values will be matched based on the
  names. Use `0` to disable smoothing and `1` to enable it. If it
  contains only one integer it will be associated to all existing
  series.

## Value

An `ms_chart` object.

## See also

Other Series customization functions:
[`chart_data_fill()`](https://ardata-fr.github.io/mschart/reference/chart_data_fill.md),
[`chart_data_line_style()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_style.md),
[`chart_data_line_width()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_width.md),
[`chart_data_size()`](https://ardata-fr.github.io/mschart/reference/chart_data_size.md),
[`chart_data_stroke()`](https://ardata-fr.github.io/mschart/reference/chart_data_stroke.md),
[`chart_data_symbol()`](https://ardata-fr.github.io/mschart/reference/chart_data_symbol.md),
[`chart_labels_text()`](https://ardata-fr.github.io/mschart/reference/chart_labels_text.md)

## Examples

``` r
linec <- ms_linechart(data = iris, x = "Sepal.Length",
  y = "Sepal.Width", group = "Species")
linec <- chart_data_smooth(linec,
  values = c(virginica = 0, versicolor = 0, setosa = 0) )
```
