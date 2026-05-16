# Modify labels font settings

Specify mappings from levels in the data to displayed text font
settings.

## Usage

``` r
chart_labels_text(x, values)
```

## Arguments

- x:

  an `ms_chart` object.

- values:

  a named list of
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html)
  objects to map data labels to. It is a named list, the values will be
  matched based on the names. If it contains only one
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html)
  object, it will be associated to all existing series.

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
[`chart_data_symbol()`](https://ardata-fr.github.io/mschart/reference/chart_data_symbol.md)

## Examples

``` r
library(officer)

fp_text_settings <- list(
  serie1 = fp_text(font.size = 7, color = "red"),
  serie2 = fp_text(font.size = 0, color = "purple"),
  serie3 = fp_text(font.size = 19, color = "wheat")
)

barchart <- ms_barchart(
  data = browser_data,
  x = "browser", y = "value", group = "serie")
barchart <- chart_data_labels(barchart, show_val = TRUE)
barchart <- chart_labels_text( barchart,
  values = fp_text_settings )
```
