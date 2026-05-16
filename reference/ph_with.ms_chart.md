# Add a MS Chart output into a PowerPoint object

Produces a Microsoft Chart graphics output from R instructions and adds
the result in a PowerPoint document object produced by
[`officer::read_pptx()`](https://davidgohel.github.io/officer/reference/read_pptx.html).

## Usage

``` r
# S3 method for class 'ms_chart'
ph_with(x, value, location, ...)
```

## Arguments

- x:

  a pptx device

- value:

  chart object

- location:

  a location for a placeholder.

- ...:

  Arguments to be passed to methods.

## Value

An rpptx object.

## See also

[`body_add_chart()`](https://ardata-fr.github.io/mschart/reference/body_add_chart.md)

## Examples

``` r
my_barchart <- ms_barchart(data = browser_data,
  x = "browser", y = "value", group = "serie")
my_barchart <- chart_settings( x = my_barchart,
  dir="vertical", grouping="clustered", gap_width = 50 )
my_barchart <- chart_ax_x( x= my_barchart,
  cross_between = 'between', major_tick_mark="out")
my_barchart <- chart_ax_y( x= my_barchart,
  cross_between = "midCat", major_tick_mark="in")

library(officer)
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with(doc, my_barchart, location = ph_location_fullsize())

fileout <- tempfile(fileext = ".pptx")
print(doc, target = fileout)
```
