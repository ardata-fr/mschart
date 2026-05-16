# Treemap chart object

Creation of a treemap chart object that can be inserted in a 'Microsoft'
document. Treemap charts use the chartEx pipeline (Office 2016+); older
versions of 'Microsoft Office' will display a fallback placeholder.

Data is hierarchical: one column per level (parent to leaf, left to
right) and one numeric column for the leaf values.

## Usage

``` r
ms_treemapchart(data, path, value, labels = NULL)
```

## Arguments

- data:

  a data.frame.

- path:

  character vector of column names defining the hierarchy, from
  outermost (root) to innermost (leaf).

- value:

  column name for the numeric leaf values.

- labels:

  unused for now; reserved for future custom data label columns.

## Value

An `ms_chart` object (subclass `ms_treemapchart`).

## Per-leaf coloring with [`chart_data_fill()`](https://ardata-fr.github.io/mschart/reference/chart_data_fill.md)

Per-leaf coloring via
[`chart_data_fill()`](https://ardata-fr.github.io/mschart/reference/chart_data_fill.md)
with a named vector works correctly on flat treemaps
(`length(path) == 1`). With a hierarchy (`length(path) >= 2`),
PowerPoint silently re-maps `<cx:dataPt>` indices in a way that drops
`idx="0"` and applies the last specified color to remaining leaves. This
is a PowerPoint rendering limitation (also reproducible from
Excel-generated chartEx files); the mschart XML output is conformant.
For hierarchical treemaps, prefer a single fill color via
`chart_data_fill(x, "#HEX")`.

## See also

[`chart_labels()`](https://ardata-fr.github.io/mschart/reference/chart_labels.md),
[`chart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.md)

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
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/reference/ms_waterfallchart.md)

## Examples

``` r
library(officer)

dat <- data.frame(
  region  = c("EU", "EU", "EU", "AM", "AM"),
  country = c("FR", "FR", "DE", "US", "US"),
  city    = c("Paris", "Lyon", "Berlin", "NYC", "LA"),
  value   = c(10, 5, 12, 20, 8),
  stringsAsFactors = FALSE
)
tm <- ms_treemapchart(
  data = dat, path = c("region", "country", "city"), value = "value"
)
tm <- chart_labels(tm, title = "Sales by region")

doc <- read_pptx()
doc <- add_slide(doc)
#> Warning: Calling `add_slide()` without specifying a `layout` is deprecated.
#>  Please pass a `layout` or use `layout_default()` to set a default.
#>  => I will now continue with the former `layout` default "Title and Content" for backwards compatibility...
doc <- ph_with(doc, tm, location = ph_location_fullsize())
print(doc, target = tempfile(fileext = ".pptx"))
```
