# Boxplot chart object

Creation of a box-and-whisker chart object that can be inserted in a
'Microsoft' document. Boxplot charts use the chartEx pipeline (Office
2016+); older versions of 'Microsoft Office' will display a fallback
placeholder.

Data is in long format: one row per observation. Office computes
quartiles and whiskers from the raw values; do not pre-aggregate.

## Usage

``` r
ms_boxplotchart(
  data,
  x,
  y,
  quartile_method = c("exclusive", "inclusive"),
  show_mean_marker = TRUE,
  show_mean_line = FALSE,
  show_outliers = TRUE,
  show_inner_points = FALSE
)
```

## Arguments

- data:

  a data.frame.

- x:

  category column name. Each unique value becomes one box.

- y:

  numeric value column name (raw observations).

- quartile_method:

  one of `"exclusive"` (default) or `"inclusive"`. Affects how Q1/Q3 are
  computed when the count is even.

- show_mean_marker:

  logical, draw the mean as a marker. Default TRUE.

- show_mean_line:

  logical, draw a line connecting means across boxes. Default FALSE.

- show_outliers:

  logical, plot outlier points. Default TRUE.

- show_inner_points:

  logical, plot all non-outlier points. Default FALSE.

## Value

An `ms_chart` object (subclass `ms_boxplotchart`).

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
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
library(officer)

set.seed(1)
dat <- data.frame(
  group = rep(c("A", "B", "C"), each = 20),
  value = c(rnorm(20, 0, 5), rnorm(20, 3, 7), rnorm(20, -2, 4))
)
bp <- ms_boxplotchart(dat, x = "group", y = "value")

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, bp, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
