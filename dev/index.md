# mschart R package

[![R build
status](https://github.com/ardata-fr/mschart/workflows/R-CMD-check/badge.svg)](https://github.com/ardata-fr/mschart/actions)
[![version](https://www.r-pkg.org/badges/version/mschart)](https://CRAN.R-project.org/package=mschart)
[![test
coverage](https://codecov.io/gh/ardata-fr/mschart/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ardata-fr/mschart)
![Active](https://www.repostatus.org/badges/latest/active.svg)

The `mschart` package provides a framework for easily creating charts
for ‘Microsoft PowerPoint’ presentations, ‘Microsoft Word’ documents and
‘Microsoft Excel’ workbooks. It has to be used with package
[`officer`](https://davidgohel.github.io/officer/) that will produce the
charts in new or existing PowerPoint, Word or Excel files. With
‘Microsoft Charts’, the data is integrated into the document and linked
to the chart. The result can be edited, annotated and resized. If the
data is updated in the document, the chart is also updated.

## Chart types

Classical charts (Office 2007+):
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md),
[`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_scatterchart.md),
[`ms_piechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_piechart.md),
[`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md),
[`ms_radarchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_radarchart.md),
[`ms_stockchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_stockchart.md).

chartEx charts (Office 2016+):
[`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md),
[`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md),
[`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md),
[`ms_paretochart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_paretochart.md),
[`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_sunburstchart.md),
[`ms_treemapchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_treemapchart.md),
[`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_waterfallchart.md).

Classical charts can be combined on the same plot area with
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md).

## Example

This is a basic example which shows you how to create a bar chart.

``` r

library(mschart)

sales <- data.frame(
  quarter = rep(c("Q1", "Q2", "Q3", "Q4"), each = 2),
  revenue = c(12, 9, 15, 11, 18, 14, 21, 17),
  region  = rep(c("EU", "US"), times = 4)
)

bars <- ms_barchart(
  data = sales, x = "quarter", y = "revenue", group = "region"
)
```

Then use package `officer` to send the object as a chart.

``` r

library(officer)
doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = bars, location = ph_location_fullsize())

print(doc, target = "example.pptx")
```

The same chart object can be dropped into an Excel sheet with
[`officer::sheet_add_drawing()`](https://davidgohel.github.io/officer/reference/sheet_add_drawing.html).
By default the chart’s underlying data is written next to the chart
automatically.

You can also write the data first with
[`officer::sheet_write_data()`](https://davidgohel.github.io/officer/reference/sheet_write_data.html)
and ask the chart to reuse it by passing `write_data = FALSE`. This is
useful when the data should appear at a specific position, or when
several charts share the same dataset:

``` r

wb <- read_xlsx()
wb <- add_sheet(wb, label = "sales")

# 1. Write the data on the sheet at the desired position.
wb <- sheet_write_data(wb, sheet = "sales", value = bars$data_series,
                       start_col = 1, start_row = 1)

# 2. Add the chart and tell it to reference the data already in place.
wb <- sheet_add_drawing(wb, sheet = "sales", value = bars,
                        write_data = FALSE,
                        start_col = 1, start_row = 1,
                        left = 5, top = 0.5, width = 6, height = 4)

print(wb, target = "example.xlsx")
```

Use `print(your_chart, preview = TRUE)` to open the chart in PowerPoint
while iterating.

## Installation

You can get the development version from GitHub:

``` r

devtools::install_github("ardata-fr/mschart")
```

Or the latest version on CRAN:

``` r

install.packages("mschart")
```

## Contributing to the package

### Bug reports

When you file a [bug
report](https://github.com/ardata-fr/mschart/issues), please spend some
time making it easy for me to follow and reproduce. The more time you
spend on making the bug report coherent, the more time I can dedicate to
investigating the bug as opposed to the bug report.
