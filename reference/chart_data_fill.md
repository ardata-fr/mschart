# Modify fill colour

Specify mappings from levels in the data to displayed fill colours.

## Usage

``` r
chart_data_fill(x, values, update_stroke = TRUE)
```

## Arguments

- x:

  an `ms_chart` object.

- values:

  `character(num of series|1)`: a set of colour values to map data
  values to. It is a named vector, the values will be matched based on
  the names. If it contains only one colour, this colour will be
  associated to all existing series.

- update_stroke:

  if `TRUE` (the default), the series stroke colour is also updated to
  `values`, so that the filled shape and its border match. Series whose
  stroke was set to `"transparent"` (for example by the
  [`ms_areachart()`](https://ardata-fr.github.io/mschart/reference/ms_areachart.md)
  and
  [`ms_piechart()`](https://ardata-fr.github.io/mschart/reference/ms_piechart.md)
  constructors) are left untouched to preserve that deliberate "no
  border" default. Set to `FALSE` to keep the current stroke colours
  untouched and manage them independently via
  [`chart_data_stroke()`](https://ardata-fr.github.io/mschart/reference/chart_data_stroke.md).

## Value

An `ms_chart` object.

## See also

Other Series customization functions:
[`chart_data_line_style()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_style.md),
[`chart_data_line_width()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_width.md),
[`chart_data_size()`](https://ardata-fr.github.io/mschart/reference/chart_data_size.md),
[`chart_data_smooth()`](https://ardata-fr.github.io/mschart/reference/chart_data_smooth.md),
[`chart_data_stroke()`](https://ardata-fr.github.io/mschart/reference/chart_data_stroke.md),
[`chart_data_symbol()`](https://ardata-fr.github.io/mschart/reference/chart_data_symbol.md),
[`chart_labels_text()`](https://ardata-fr.github.io/mschart/reference/chart_labels_text.md)

## Examples

``` r
my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
  y = "Sepal.Width",  group = "Species")
my_scatter <- chart_data_fill(my_scatter,
  values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
```
