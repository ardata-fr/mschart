# Modify data labels settings

Data labels show details about data series. S3 generic; the `default`
method is documented below. ChartEx charts honor a leaner set of
options.

## Usage

``` r
chart_data_labels(x, ...)

# Default S3 method
chart_data_labels(
  x,
  num_fmt = "General",
  position = "ctr",
  show_legend_key = FALSE,
  show_val = FALSE,
  show_cat_name = FALSE,
  show_serie_name = FALSE,
  show_percent = FALSE,
  separator = ", ",
  ...
)
```

## Arguments

- x:

  an `ms_chart` object.

- ...:

  arguments passed to S3 methods.

- num_fmt:

  `character(1)`: number formatting specifies number format properties
  which indicate how to format and render the numeric values. It can be
  "General", "0.00", "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm",
  etc.

- position:

  `character(1)`: it specifies the position of the data label. It should
  be one of 'b', 'ctr', 'inBase', 'inEnd', 'l', 'outEnd', 'r', 't'. When
  grouping is 'clustered', it should be one of
  'ctr','inBase','inEnd','outEnd'. When grouping is 'stacked', it should
  be one of 'ctr','inBase','inEnd'. When grouping is 'standard', it
  should be one of 'b','ctr','l','r','t'.

- show_legend_key:

  show legend key if TRUE.

- show_val:

  show values if TRUE.

- show_cat_name:

  show categories if TRUE.

- show_serie_name:

  show names of series if TRUE.

- show_percent:

  show percentages if TRUE.

- separator:

  separator between the label components (value, category name, series
  name, etc.) when multiple components are displayed. Default is `", "`.

## Value

An `ms_chart` object.

## See also

[`chart_labels_text()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels_text.md),
[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

## Examples

``` r
my_bc <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
my_bc <- chart_data_labels(my_bc, show_val = TRUE, position = "outEnd")
```
