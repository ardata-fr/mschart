# Data table settings

Define visual settings for the data table displayed below the chart.
Requires `chart_settings(x, table = TRUE)` to be called first.

## Usage

``` r
chart_table(x, horizontal, vertical, outline, show_keys)
```

## Arguments

- x:

  an `ms_chart` object.

- horizontal:

  write horizontal lines in the table

- vertical:

  write vertical lines in the table

- outline:

  write an outline in the table

- show_keys:

  show keys in the table

## Value

An `ms_chart` object.

## See also

[`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md)

## Examples

``` r
data <- data.frame(
  supp = factor(rep(c("OJ", "VC"), each = 3),
                levels = c("OJ", "VC")),
  dose = factor(rep(c("low", "medium", "high"), 2),
                levels = c("low", "medium", "high")),
  length = c(13.23, 22.7, 24.06, 7.98, 16.77, 26.14),
  label = LETTERS[1:6],
  stringsAsFactors = FALSE
)

# example chart 03 -------
chart <- ms_linechart(
  data = data, x = "dose", y = "length",
  group = "supp", labels = "label"
)
chart <- chart_settings(
  x = chart, table = TRUE
)

chart <- chart_table(chart,
  horizontal = TRUE, vertical = FALSE,
  outline = TRUE, show_keys = FALSE
)
```
