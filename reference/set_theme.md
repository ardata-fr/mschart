# Set chart theme

Modify chart theme with function `set_theme`.

Use `mschart_theme()` to create a chart theme.

Use `chart_theme()` to modify components of the theme of a chart.

## Usage

``` r
set_theme(x, value)

mschart_theme(
  axis_title = fp_text(bold = TRUE, font.size = 16),
  axis_title_x = axis_title,
  axis_title_y = axis_title,
  main_title = fp_text(bold = TRUE, font.size = 20),
  legend_text = fp_text(font.size = 14),
  table_text = fp_text(bold = FALSE, font.size = 9),
  axis_text = fp_text(),
  axis_text_x = axis_text,
  axis_text_y = axis_text,
  title_rot = 0,
  title_x_rot = 0,
  title_y_rot = 270,
  axis_ticks = fp_border(color = "#99999999"),
  axis_ticks_x = axis_ticks,
  axis_ticks_y = axis_ticks,
  grid_major_line = fp_border(color = "#99999999", style = "dashed"),
  grid_major_line_x = grid_major_line,
  grid_major_line_y = grid_major_line,
  grid_minor_line = fp_border(width = 0),
  grid_minor_line_x = grid_minor_line,
  grid_minor_line_y = grid_minor_line,
  chart_background = NULL,
  chart_border = fp_border(color = "transparent"),
  plot_background = NULL,
  plot_border = fp_border(color = "transparent"),
  date_fmt = "yyyy/mm/dd",
  str_fmt = "General",
  double_fmt = "#,##0.00",
  integer_fmt = "0",
  legend_position = "b",
  legend_x = NULL,
  legend_y = NULL,
  legend_w = NULL,
  legend_h = NULL
)

chart_theme(
  x,
  axis_title_x,
  axis_title_y,
  main_title,
  legend_text,
  title_rot,
  title_x_rot,
  title_y_rot,
  axis_text_x,
  axis_text_y,
  axis_ticks_x,
  axis_ticks_y,
  grid_major_line_x,
  grid_major_line_y,
  grid_minor_line_x,
  grid_minor_line_y,
  chart_background,
  chart_border,
  plot_background,
  plot_border,
  date_fmt,
  str_fmt,
  double_fmt,
  integer_fmt,
  legend_position,
  legend_x,
  legend_y,
  legend_w,
  legend_h
)
```

## Arguments

- x:

  an `ms_chart` object.

- value:

  a `mschart_theme()` object.

- axis_title, axis_title_x, axis_title_y:

  axis title formatting properties (see
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html))

- main_title:

  title formatting properties (see
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html))

- legend_text:

  legend text formatting properties (see
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html))

- table_text:

  table text formatting properties (see
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html))

- axis_text, axis_text_x, axis_text_y:

  axis text formatting properties (see
  [`officer::fp_text()`](https://davidgohel.github.io/officer/reference/fp_text.html))

- title_rot, title_x_rot, title_y_rot:

  rotation angle

- axis_ticks, axis_ticks_x, axis_ticks_y:

  axis ticks formatting properties (see
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html))

- grid_major_line, grid_major_line_x, grid_major_line_y:

  major grid lines formatting properties (see
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html))

- grid_minor_line, grid_minor_line_x, grid_minor_line_y:

  minor grid lines formatting properties (see
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html))

- chart_background:

  chart area background fill color - single character value (e.g.
  "#000000" or "black")

- chart_border:

  chart area border lines formatting properties (see
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html))

- plot_background:

  plot area background fill color - single character value (e.g.
  "#000000" or "black")

- plot_border:

  plot area border lines formatting properties (see
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html))

- date_fmt:

  date format

- str_fmt:

  string or factor format

- double_fmt:

  double format

- integer_fmt:

  integer format

- legend_position:

  it specifies the position of the legend. It should be one of 'b',
  'tr', 'l', 'r', 't', 'n' (for 'none').

- legend_x, legend_y, legend_w, legend_h:

  optional fractions between 0 and 1 to manually position and size the
  legend box within the chart area. Each value is a fraction of the
  chart width (`legend_x`, `legend_w`) or height (`legend_y`,
  `legend_h`). Any `NULL` value keeps the default automatic layout;
  setting at least one triggers manual placement via `<c:manualLayout>`.

## Value

An `ms_chart` object.

An `mschart_theme` object (for `mschart_theme()`).

An `ms_chart` object (for `set_theme()` and `chart_theme()`).

## See also

[`ms_barchart()`](https://ardata-fr.github.io/mschart/reference/ms_barchart.md),
[`ms_areachart()`](https://ardata-fr.github.io/mschart/reference/ms_areachart.md),
[`ms_scatterchart()`](https://ardata-fr.github.io/mschart/reference/ms_scatterchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/reference/ms_linechart.md)

## Examples

``` r
library(officer)
mytheme <- mschart_theme(
  axis_title = fp_text(color = "red", font.size = 24, bold = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 0.4, color = "gray")
)


my_bc <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
my_bc <- chart_settings(my_bc,
  dir = "horizontal", grouping = "stacked",
  gap_width = 150, overlap = 100
)
my_bc <- set_theme(my_bc, mytheme)

my_bc_2 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
my_bc_2 <- chart_theme(my_bc_2,
  grid_major_line_y = fp_border(width = 0.5, color = "cyan")
)

# Manual legend layout: place the legend in the top-right corner
# using fractions of the chart area (0 to 1).
my_bc_3 <- ms_barchart(
  data = browser_data, x = "browser",
  y = "value", group = "serie"
)
my_bc_3 <- chart_theme(my_bc_3,
  legend_position = "r",
  legend_x = 0.80, legend_y = 0.15,
  legend_w = 0.18, legend_h = 0.30
)
```
