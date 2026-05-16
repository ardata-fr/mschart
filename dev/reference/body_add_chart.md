# Add a chart to a Word document

Add a `ms_chart` to an rdocx object. The graphic will be inserted in an
empty paragraph.

## Usage

``` r
body_add_chart(x, chart, style = NULL, pos = "after", width = 5, height = 3)
```

## Arguments

- x:

  an rdocx object

- chart:

  an `ms_chart` object.

- style:

  paragraph style

- pos:

  where to add the new element relative to the cursor, one of "after",
  "before", "on".

- height, width:

  height and width in inches.

## Value

An rdocx object.

## See also

[`ph_with.ms_chart()`](https://ardata-fr.github.io/mschart/dev/reference/ph_with.ms_chart.md)

## Examples

``` r
library(officer)
my_barchart <- ms_barchart(data = browser_data,
  x = "browser", y = "value", group = "serie")
my_barchart <- chart_settings( my_barchart, grouping = "stacked",
  gap_width = 50, overlap = 100 )

doc <- read_docx()
doc <- body_add_chart(doc, chart = my_barchart, style = "centered")
print(doc, target = tempfile(fileext = ".docx"))
```
