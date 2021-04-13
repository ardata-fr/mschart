mschart R package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R build
status](https://github.com/ardata-fr/mschart/workflows/R-CMD-check/badge.svg)](https://github.com/ardata-fr/mschart/actions)
[![version](https://www.r-pkg.org/badges/version/mschart)](https://CRAN.R-project.org/package=mschart)
![cranlogs](https://cranlogs.r-pkg.org/badges/mschart)
![Active](http://www.repostatus.org/badges/latest/active.svg)

<a href="https://github.com/ardata-fr/mschart"><img src="man/figures/logo.png" alt="mschart logo" align="right" /></a>
The `mschart` package provides a framework for easily create charts for
‘Microsoft PowerPoint’ presentations and ‘Microsoft Word’ documents. It
has to be used with package
[`officer`](https://davidgohel.github.io/officer/) that will produce the
charts in new or existing PowerPoint or Word documents. With ‘Microsoft
Charts’, the data is integrated into the document and linked to the
chart. The result can be edited, annotated and resized. If the data is
updated in the document, the chart is also updated.

## Example

This is a basic example which shows you how to create a scatter plot.

``` r
library(mschart)
scatter <-
  ms_scatterchart(
    data = iris, x = "Sepal.Length",
    y = "Sepal.Width", group = "Species"
  )
scatter <- chart_settings(scatter, scatterstyle = "marker")
```

Then use package `officer` to send the object as a chart.

``` r
library(officer)
doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, value = scatter, location = ph_location_fullsize())

print(doc, target = "example.pptx")
```

![](man/figures/README-example.png)

At any moment, you can type `print(your_chart, preview = TRUE)` to
preview the chart in a temporary PowerPoint file. This requires to have
a PowerPoint Viewer installed on the machine.

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
investigate the bug as opposed to the bug report.
