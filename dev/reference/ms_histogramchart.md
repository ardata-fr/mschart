# Histogram chart object

Creation of a histogram chart object that can be inserted in a
'Microsoft' document. Histogram charts use the chartEx pipeline (Office
2016+); older versions of 'Microsoft Office' will display a fallback
placeholder.

Data is a single column of raw numeric observations. Office computes the
bins automatically; pass `bin_count` or `bin_width` to override.

## Usage

``` r
ms_histogramchart(
  data,
  value,
  bin_count = NULL,
  bin_width = NULL,
  interval_closed = c("right", "left"),
  underflow = NULL,
  overflow = NULL
)
```

## Arguments

- data:

  a data.frame.

- value:

  numeric column name (raw observations).

- bin_count:

  integer, requested number of bins. Mutually exclusive with
  `bin_width`. NULL = automatic.

- bin_width:

  numeric, requested bin width. Mutually exclusive with `bin_count`.
  NULL = automatic.

- interval_closed:

  one of `"right"` (default) or `"left"`. Defines which end of each bin
  is inclusive.

- underflow:

  numeric, values below this go in an "underflow" bin. NULL to disable.

- overflow:

  numeric, values above this go in an "overflow" bin. NULL to disable.

## Value

An `ms_chart` object (subclass `ms_histogramchart`).

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
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
dat <- data.frame(x = rnorm(200, mean = 10, sd = 4))
hi <- ms_histogramchart(dat, value = "x", bin_count = 12)

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, hi, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
