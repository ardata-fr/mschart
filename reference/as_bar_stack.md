# Set a barchart as a stacked barchart

Apply settings to an `ms_barchart` object to produce a stacked barchart.
Options are available to use percentage instead of values and to choose
if bars should be vertically or horizontally drawn.

## Usage

``` r
as_bar_stack(x, dir = "vertical", percent = FALSE, gap_width = 50)
```

## Arguments

- x:

  an
  [`ms_barchart`](https://ardata-fr.github.io/mschart/reference/ms_barchart.md)
  object

- dir:

  the direction of the bars in the chart, value must be one of
  "horizontal" or "vertical".

- percent:

  should bars be displayed as percentages.

- gap_width:

  gap width between bars for each category on a bar chart, as a
  percentage of the bar width. It can be set between 0 and 500.

## Value

An `ms_chart` object.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md),
[`ms_barchart()`](https://ardata-fr.github.io/mschart/reference/ms_barchart.md)

## Examples

``` r
library(officer)

my_bar_stack_01 <- ms_barchart(data = browser_data, x = "browser",
  y = "value", group = "serie")
my_bar_stack_01 <- as_bar_stack( my_bar_stack_01 )

my_bar_stack_02 <- ms_barchart(data = browser_data, x = "browser",
  y = "value", group = "serie")
my_bar_stack_02 <- as_bar_stack( my_bar_stack_02, percent = TRUE,
  dir = "horizontal" )

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, my_bar_stack_02, location = ph_location_fullsize())

fileout <- tempfile(fileext = ".pptx")
print(doc, target = fileout)
```
