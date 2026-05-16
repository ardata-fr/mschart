# Funnel chart object

Creation of a funnel chart object that can be inserted in a 'Microsoft'
document. Funnel charts use the chartEx pipeline (Office 2016+); older
versions of 'Microsoft Office' will display a fallback placeholder.

Each row is one stage of the funnel. Values are typically decreasing
(e.g. visitors -\> leads -\> customers). Bars are centered horizontally
and width is proportional to the value.

## Usage

``` r
ms_funnelchart(data, x, y)
```

## Arguments

- data:

  a data.frame.

- x:

  category column name (stage label).

- y:

  numeric value column name.

## Value

An `ms_chart` object (subclass `ms_funnelchart`).

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md)

Other 'Office' chart objects:
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md),
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

dat <- data.frame(
  stage = c("Visitors", "Leads", "Opportunities", "Quotes", "Customers"),
  count = c(5000, 4000, 3000, 1000, 250),
  stringsAsFactors = FALSE
)
fn <- ms_funnelchart(data = dat, x = "stage", y = "count")

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, fn, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
