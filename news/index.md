# Changelog

## mschart 0.5.0

### Highlights

- Seven new chart types built on Office’s chartEx family (Office 2016+):
  box-and-whisker
  ([`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/reference/ms_boxplotchart.md)),
  funnel
  ([`ms_funnelchart()`](https://ardata-fr.github.io/mschart/reference/ms_funnelchart.md)),
  histogram
  ([`ms_histogramchart()`](https://ardata-fr.github.io/mschart/reference/ms_histogramchart.md)),
  pareto
  ([`ms_paretochart()`](https://ardata-fr.github.io/mschart/reference/ms_paretochart.md)),
  sunburst
  ([`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/reference/ms_sunburstchart.md)),
  treemap
  ([`ms_treemapchart()`](https://ardata-fr.github.io/mschart/reference/ms_treemapchart.md))
  and waterfall
  ([`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/reference/ms_waterfallchart.md)).

- Place a chart on an Excel sheet anchored to cells. Pass
  `anchor = "B2:H20"` to
  [`sheet_add_drawing()`](https://davidgohel.github.io/officer/reference/sheet_add_drawing.html)
  to anchor between two cells (Excel’s default “move and size with
  cells”), or `anchor = "B2"` to anchor to a single cell (move only).
  Omit `anchor` for absolute placement. `edit_as` controls what Excel
  does when rows or columns are resized.

### Bug fixes

- Custom `num_fmt` strings containing XML metacharacters (e.g. the
  `[>=1000]` conditional Excel format) no longer break chart XML
  generation; they are escaped automatically
  ([\#98](https://github.com/ardata-fr/mschart/issues/98)).
- [`ms_stockchart()`](https://ardata-fr.github.io/mschart/reference/ms_stockchart.md)
  no longer fails when the `x` column is literally named `group` (or
  `x_val` / `y_val`). Internal column names now use a `.mschart_` prefix
  to avoid collisions with user data.
- [`ms_chart_combine()`](https://ardata-fr.github.io/mschart/reference/ms_chart_combine.md)
  supports independent x ranges via `secondary_x` (top axis), in
  addition to the existing `secondary_y` mode.
- [`ms_chart_combine()`](https://ardata-fr.github.io/mschart/reference/ms_chart_combine.md)
  no longer produces files PowerPoint refuses to open, and reports clear
  errors on incompatible inputs (column collisions, x mismatches,
  unsupported axis combinations).

## mschart 0.4.3

CRAN release: 2026-04-25

### Highlights

- Four new chart types: pie / doughnut
  ([`ms_piechart()`](https://ardata-fr.github.io/mschart/reference/ms_piechart.md)),
  bubble
  ([`ms_bubblechart()`](https://ardata-fr.github.io/mschart/reference/ms_bubblechart.md)),
  radar
  ([`ms_radarchart()`](https://ardata-fr.github.io/mschart/reference/ms_radarchart.md))
  and stock charts with HLC and OHLC candlesticks
  ([`ms_stockchart()`](https://ardata-fr.github.io/mschart/reference/ms_stockchart.md)).
- Combine several chart types (e.g. bars + lines) on a single chart,
  with optional secondary axis, via
  [`ms_chart_combine()`](https://ardata-fr.github.io/mschart/reference/ms_chart_combine.md).
- Drop a chart into an Excel sheet with
  [`officer::sheet_add_drawing()`](https://davidgohel.github.io/officer/reference/sheet_add_drawing.html);
  chart data is written into the sheet and linked automatically.

### New features

#### Charts

- Line charts can now be stacked or percent-stacked
  (`chart_settings(grouping = ...)`).
- Stock charts gain the data-table option already available on bar /
  line / area charts (`chart_settings(table = TRUE)`).

#### Axes, theme and legend

- Control axis interval spacing via `major_unit`, `minor_unit`,
  `major_time_unit` and `minor_time_unit`
  ([\#105](https://github.com/ardata-fr/mschart/issues/105)).
- Position and size the legend manually with `legend_x`, `legend_y`,
  `legend_w`, `legend_h` in
  [`mschart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.md)
  /
  [`chart_theme()`](https://ardata-fr.github.io/mschart/reference/set_theme.md)
  ([\#38](https://github.com/ardata-fr/mschart/issues/38)).
- Disable grid lines by setting `grid_major_line_x` / `_y` or
  `grid_minor_line_x` / `_y` to `FALSE`.

#### Series styling

- [`chart_data_fill()`](https://ardata-fr.github.io/mschart/reference/chart_data_fill.md)
  updates the matching stroke colour by default, so one call produces a
  filled shape with a matching border. Opt out with
  `update_stroke = FALSE`.

#### Validation and warnings

- Series styling functions now warn when applied to a chart type that
  does not support the property (see
  [`?mschart`](https://ardata-fr.github.io/mschart/reference/mschart.md)
  for the supported matrix).
- `chart_settings(table = TRUE)` warns instead of being silently ignored
  on chart types without a data table (scatter, radar, bubble, pie).
- [`chart_labels()`](https://ardata-fr.github.io/mschart/reference/chart_labels.md)
  validates `title`, `xlab` and `ylab`: each must be `NULL` or a single
  non-NA string.

### Bug fixes

- Thin lines below 1pt (0.25, 0.5, 0.75) are rendered on
  `ms_scatterchart` like on other chart types.
- `chart_data_line_style("none")` actually hides the line
  ([\#91](https://github.com/ardata-fr/mschart/issues/91),
  [\#99](https://github.com/ardata-fr/mschart/issues/99), thanks Stefan
  Moog).
- Theme number formats (`date_fmt`, `double_fmt`, …) are applied to axes
  again.
- [`chart_data_smooth()`](https://ardata-fr.github.io/mschart/reference/chart_data_smooth.md)
  resolves the right series.
- [`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md)
  no longer resets unrelated options when called with partial parameters
  (linechart, areachart, scatterchart).
- Fixed example in
  [`?chart_data_size`](https://ardata-fr.github.io/mschart/reference/chart_data_size.md).

### Breaking changes

- [`chart_settings()`](https://ardata-fr.github.io/mschart/reference/chart_settings.md)
  on `ms_bubblechart` no longer accepts `style` — it was silently
  ignored before. Remove the argument from your code.

## mschart 0.4.1

CRAN release: 2025-08-18

### Issues

- fix issue with dcast by making sure all data are preserved.

### New features

- Add support to set chart and plot area color and border by Stefan Moog
  ; added four new theme arguments chart/plot_background and
  chart/plot_border, new theme
  [`theme_ggplot2()`](https://ardata-fr.github.io/mschart/reference/theme_ggplot2.md).

## mschart 0.4.0

CRAN release: 2022-11-30

### New features

- Support for openxlsx2 by Jan Marvin Garbuszus
- option to add a table of data below the chart by Marlon Molina

### Issues

- fix issue with % in labels of the graphic
- stop reordering data when a group is used; the user is expected to do
  it before sending the data to mschart.

## mschart 0.3.1

CRAN release: 2021-09-02

### New features

- Support for linechart with or without lines and with or without
  markers

### Issues

- fix issue with scattercharts and ordering in the Excel dataset
- fix issue with scattercharts when points should be joined by a line

## mschart 0.3.0

CRAN release: 2021-04-19

### Issues

- fix UTF8 encoding issue for Windows users.

### New features

- Support for custom labels, this is automatically set when argument
  `labels` is used.

### Changes

- Documentation is now here:
  <https://ardata-fr.github.io/officeverse/charts-with-mschart.html>

## mschart 0.2.6

CRAN release: 2021-04-07

### Issues

- fix issue with labels that contains `<>&`.
- fix areachart that should never have a position defined for labels in
  XML series
- fix
  [`chart_labels_text()`](https://ardata-fr.github.io/mschart/reference/chart_labels_text.md)
  when only a `fp_text` was used as value for arg `values`.

### Changes

- Mark functions `ph_with_chart` and `ph_with_chart_at` as defunct.
- The R6 package is no longer used.

## mschart 0.2.5

CRAN release: 2019-11-14

### Changes

- implement method ph_with.ms_chart that is replacing `ph_with_chart`
  and `ph_with_chart_at`.

## mschart 0.2.4

CRAN release: 2019-06-26

### Enhancements

- new function
  [`chart_data_smooth()`](https://ardata-fr.github.io/mschart/reference/chart_data_smooth.md)
  to activate line smooth

### Changes

- drop shadows effects on charts

### Issues

- activate *no shadow* mode in all components.

## mschart 0.2.3

CRAN release: 2018-04-19

### Enhancements

- new function
  [`chart_labels_text()`](https://ardata-fr.github.io/mschart/reference/chart_labels_text.md)
  to customise text labels

### Issues

- fix issue 22 with grid lines and fp_border(style = “none”).

## mschart 0.2.2

CRAN release: 2017-12-12

### Issues

- htmlEscape characters to allow “&” and “\<” symbols
- num_fmt issues with `%`
- add controls and fix chart_settings.ms_scatterchart

### Enhancements

- added argument `legend_text` to theme function
- legend can be dropped now
- `ms_linechart` now accepts a non-numeric x axis.

## mschart 0.2.1

CRAN release: 2017-09-15

- Fix issue that corrupted the file when data had missing values
