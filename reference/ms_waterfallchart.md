# Waterfall chart object

Creation of a waterfall chart object that can be inserted in a
'Microsoft' document. Waterfall charts use the chartEx pipeline (Office
2016+); older versions of 'Microsoft Office' will display a fallback
placeholder.

Each row is one bar. Positive values rise, negative values fall.
Categories listed in `subtotals` are rendered as absolute totals
(typical for "Start", intermediate totals, and "End" bars).

## Usage

``` r
ms_waterfallchart(data, x, y, subtotals = NULL)
```

## Arguments

- data:

  a data.frame.

- x:

  category column name.

- y:

  numeric value column name. Signed values: positive = up, negative =
  down. For subtotal rows, the value should be the running total at that
  point.

- subtotals:

  integer vector of 1-based row indices that should be rendered as
  subtotal/total bars. Defaults to none.

## Value

An `ms_chart` object (subclass `ms_waterfallchart`).

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/reference/ms_areachart.md),
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
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/reference/ms_treemapchart.md)

## Examples

``` r
library(officer)

dat <- data.frame(
  step = c("Start", "Q1", "Q2", "Q3", "End"),
  amount = c(100, 30, -20, 40, 150),
  stringsAsFactors = FALSE
)
wf <- ms_waterfallchart(
  data = dat, x = "step", y = "amount",
  subtotals = c(1, 5)
)

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, wf, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
