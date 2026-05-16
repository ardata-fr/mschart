# Add an ms_chart to an Excel sheet

Add an `ms_chart` object to a sheet in an xlsx workbook created with
[`officer::read_xlsx()`](https://davidgohel.github.io/officer/reference/read_xlsx.html).
The chart data is written into the sheet and the chart is displayed at
the specified position.

## Usage

``` r
# S3 method for class 'ms_chart'
sheet_add_drawing(
  x,
  value,
  sheet,
  start_col = 1L,
  start_row = 1L,
  write_data = TRUE,
  left = 1,
  top = 1,
  width = 6,
  height = 4,
  anchor = NULL,
  edit_as = c("twoCell", "oneCell", "absolute"),
  ...
)
```

## Arguments

- x:

  an rxlsx object (created by
  [`officer::read_xlsx()`](https://davidgohel.github.io/officer/reference/read_xlsx.html))

- value:

  an `ms_chart` object

- sheet:

  sheet name where the chart and its data will be placed. The sheet must
  already exist (see
  [`officer::add_sheet()`](https://davidgohel.github.io/officer/reference/add_sheet.html)).

- start_col:

  column index where chart data will be written (default 1, i.e. column
  A). When `write_data = FALSE`, this is still the cell position that
  the chart XML will point at, but no data is written.

- start_row:

  row index where chart data will be written (default 1). Same semantics
  with `write_data = FALSE` as for `start_col`.

- write_data:

  if `TRUE` (the default), the chart's `data_series` is written into the
  sheet at `(start_col, start_row)` before the chart is added. Pass
  `FALSE` when the data is already present in the sheet (for example to
  avoid rewriting it when several charts share the same dataset, or when
  inserting a chart that references data written independently via
  [`officer::sheet_write_data()`](https://davidgohel.github.io/officer/reference/sheet_write_data.html)).
  Not to be confused with the constructor's `asis` argument (see
  [`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md)),
  which controls how the input data frame is read at construction time.
  The two are independent.

- left, top:

  top-left anchor of the chart, in inches. Defaults to `(1, 1)`. Used
  when `anchor = NULL`. Same convention as
  [`officer::sheet_add_drawing()`](https://davidgohel.github.io/officer/reference/sheet_add_drawing.html)
  and `rvg::sheet_add_drawing.dml()`.

- width, height:

  size of the chart, in inches. Defaults to `6 x 4`. Used when
  `anchor = NULL` or when `anchor` is a single cell reference.

- anchor:

  optional Excel cell-based anchor. Either `NULL` (the default; absolute
  placement via `left`/`top`/`width`/`height`), a single cell reference
  like `"B2"` (the chart is anchored to that cell and keeps
  `width`/`height`, i.e. "Move but don't size with cells"), or a range
  like `"B2:H20"` (the chart is anchored from the first cell to the
  second, i.e. Excel's default "Move and size with cells").

- edit_as:

  one of `"twoCell"`, `"oneCell"`, `"absolute"`. Sets the `editAs`
  attribute on `<xdr:twoCellAnchor>`. Ignored unless `anchor` is a
  range.

- ...:

  unused

## Value

the rxlsx object (invisibly)

## Examples

``` r
library(officer)
library(mschart)

my_chart <- ms_barchart(
  data = data.frame(
    x = c("A", "B", "C"),
    y = c(1, 3, 2),
    group = rep("serie1", 3)
  ),
  x = "x", y = "y", group = "group"
)

x <- read_xlsx()
x <- add_sheet(x, label = "chart_sheet")
x <- sheet_add_drawing(x, value = my_chart, sheet = "chart_sheet")
print(x, target = tempfile(fileext = ".xlsx"))

# Sharing one dataset between several charts on the same sheet:
# write the data once, then add each chart with write_data = FALSE.
shared <- data.frame(
  x = c("A", "B", "C"),
  y = c(1, 3, 2),
  group = rep("serie1", 3)
)
chart_a <- ms_barchart(shared, x = "x", y = "y", group = "group")
chart_b <- ms_linechart(shared, x = "x", y = "y", group = "group")

x <- read_xlsx()
x <- add_sheet(x, label = "multi")
x <- sheet_write_data(x, value = chart_a$data_series, sheet = "multi")
x <- sheet_add_drawing(x, value = chart_a, sheet = "multi",
                       write_data = FALSE,
                       left = 3, top = 0.5, width = 5, height = 3.5)
x <- sheet_add_drawing(x, value = chart_b, sheet = "multi",
                       write_data = FALSE,
                       left = 9, top = 0.5, width = 5, height = 3.5)
print(x, target = tempfile(fileext = ".xlsx"))
```
