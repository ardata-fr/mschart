# Modify symbol

Specify mappings from levels in the data to displayed symbols.

## Usage

``` r
chart_data_symbol(x, values)
```

## Arguments

- x:

  an `ms_chart` object.

- values:

  `character(num of series)`: a set of symbol values to map data values
  to. It is a named vector, the values will be matched based on the
  names. Possible values are: 'circle', 'dash', 'diamond', 'dot',
  'none', 'plus', 'square', 'star', 'triangle', 'x', 'auto'. If it
  contains only one symbol, this symbol will be associated to all
  existing series.

## Value

An `ms_chart` object.

## See also

Other Series customization functions:
[`chart_data_fill()`](https://ardata-fr.github.io/mschart/reference/chart_data_fill.md),
[`chart_data_line_style()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_style.md),
[`chart_data_line_width()`](https://ardata-fr.github.io/mschart/reference/chart_data_line_width.md),
[`chart_data_size()`](https://ardata-fr.github.io/mschart/reference/chart_data_size.md),
[`chart_data_smooth()`](https://ardata-fr.github.io/mschart/reference/chart_data_smooth.md),
[`chart_data_stroke()`](https://ardata-fr.github.io/mschart/reference/chart_data_stroke.md),
[`chart_labels_text()`](https://ardata-fr.github.io/mschart/reference/chart_labels_text.md)

## Examples

``` r
my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
  y = "Sepal.Width",  group = "Species")
my_scatter <- chart_data_fill(my_scatter,
  values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
my_scatter <- chart_data_stroke(my_scatter,
  values = c(virginica = "black", versicolor = "black", setosa = "black") )
my_scatter <- chart_data_symbol(my_scatter,
  values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
```
