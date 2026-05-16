# Pareto chart object

Creation of a Pareto chart object that can be inserted in a 'Microsoft'
document. Pareto charts use the chartEx pipeline (Office 2016+); older
versions of 'Microsoft Office' will display a fallback placeholder.

Office draws columns sorted by descending count plus a cumulative line
on a secondary percentage axis.

Two input modes are supported:

- `aggregate = TRUE` (default): `data` is in long format (one row per
  observation) and Office counts occurrences of each `x` value. `y` is
  optional; when supplied, values are summed per category.

- `aggregate = FALSE`: `data` is already aggregated (one row per
  category) and `y` is the count/value column.

## Usage

``` r
ms_paretochart(data, x, y = NULL, aggregate = TRUE)
```

## Arguments

- data:

  a data.frame.

- x:

  category column name.

- y:

  optional numeric column. With `aggregate=TRUE` and `y=NULL`, each row
  counts as 1.

- aggregate:

  logical, see Description. Default TRUE.

## Value

An `ms_chart` object (subclass `ms_paretochart`).

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md),
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
  defect = sample(c("A", "B", "C", "D"), 50, replace = TRUE,
                  prob = c(0.5, 0.25, 0.15, 0.1))
)
pa <- ms_paretochart(dat, x = "defect")

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, pa, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
