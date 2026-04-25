# mschart 0.4.4

## Bug fixes

* Custom `num_fmt` strings containing XML metacharacters (e.g. the
  `[>=1000]` conditional Excel format) no longer break chart XML
  generation; they are escaped automatically (#98).


# mschart 0.4.3

## Highlights

* Four new chart types: pie / doughnut (`ms_piechart()`), bubble
  (`ms_bubblechart()`), radar (`ms_radarchart()`) and stock charts with
  HLC and OHLC candlesticks (`ms_stockchart()`).
* Combine several chart types (e.g. bars + lines) on a single chart,
  with optional secondary axis, via `ms_chart_combine()`.
* Drop a chart into an Excel sheet with `officer::sheet_add_drawing()`;
  chart data is written into the sheet and linked automatically.

## New features

### Charts

* Line charts can now be stacked or percent-stacked
  (`chart_settings(grouping = ...)`).
* Stock charts gain the data-table option already available on
  bar / line / area charts (`chart_settings(table = TRUE)`).

### Axes, theme and legend

* Control axis interval spacing via `major_unit`, `minor_unit`,
  `major_time_unit` and `minor_time_unit` (#105).
* Position and size the legend manually with `legend_x`, `legend_y`,
  `legend_w`, `legend_h` in `mschart_theme()` / `chart_theme()` (#38).
* Disable grid lines by setting `grid_major_line_x` / `_y` or
  `grid_minor_line_x` / `_y` to `FALSE`.

### Series styling

* `chart_data_fill()` updates the matching stroke colour by default,
  so one call produces a filled shape with a matching border. Opt out
  with `update_stroke = FALSE`.

### Validation and warnings

* Series styling functions now warn when applied to a chart type that
  does not support the property (see `?mschart` for the supported
  matrix).
* `chart_settings(table = TRUE)` warns instead of being silently
  ignored on chart types without a data table (scatter, radar, bubble,
  pie).
* `chart_labels()` validates `title`, `xlab` and `ylab`: each must be
  `NULL` or a single non-NA string.

## Bug fixes

* Thin lines below 1pt (0.25, 0.5, 0.75) are rendered on
  `ms_scatterchart` like on other chart types.
* `chart_data_line_style("none")` actually hides the line (#91, #99,
  thanks Stefan Moog).
* Theme number formats (`date_fmt`, `double_fmt`, ...) are applied to
  axes again.
* `chart_data_smooth()` resolves the right series.
* `chart_settings()` no longer resets unrelated options when called
  with partial parameters (linechart, areachart, scatterchart).
* Fixed example in `?chart_data_size`.

## Breaking changes

* `chart_settings()` on `ms_bubblechart` no longer accepts `style` —
  it was silently ignored before. Remove the argument from your code.

# mschart 0.4.1

## Issues

* fix issue with dcast by making sure all data are preserved.

## New features

* Add support to set chart and plot area color and border by Stefan Moog ;
added four new theme arguments chart/plot_background and chart/plot_border, 
new theme `theme_ggplot2()`.



# mschart 0.4.0

## New features

* Support for openxlsx2 by Jan Marvin Garbuszus
* option to add a table of data below the chart by Marlon Molina

## Issues

* fix issue with % in labels of the graphic
* stop reordering data when a group is used; the user is expected to
do it before sending the data to mschart.

# mschart 0.3.1

## New features

* Support for linechart with or without lines and with or without markers

## Issues

* fix issue with scattercharts and ordering in the Excel dataset
* fix issue with scattercharts when points should be joined by a line

# mschart 0.3.0

## Issues

* fix UTF8 encoding issue for Windows users.

## New features

* Support for custom labels, this is automatically set when argument `labels` is used.

## Changes

* Documentation is now here: https://ardata-fr.github.io/officeverse/charts-with-mschart.html

# mschart 0.2.6

## Issues

* fix issue with labels that contains `<>&`.
* fix areachart that should never have a position defined for labels in XML series
* fix `chart_labels_text()` when only a `fp_text` was used as value for arg `values`.

## Changes

* Mark functions `ph_with_chart` and `ph_with_chart_at` as defunct.
* The R6 package is no longer used.

# mschart 0.2.5

## Changes

* implement method ph_with.ms_chart that is replacing `ph_with_chart` and 
  `ph_with_chart_at`.

# mschart 0.2.4

## Enhancements

* new function `chart_data_smooth()` to activate line smooth

## Changes

* drop shadows effects on charts

## Issues

* activate *no shadow* mode in all components.

# mschart 0.2.3

## Enhancements

* new function `chart_labels_text()` to customise text labels

## Issues

* fix issue 22 with grid lines and fp_border(style = "none").

# mschart 0.2.2

## Issues

* htmlEscape characters to allow "&" and "<" symbols
* num_fmt issues with `%`
* add controls and fix chart_settings.ms_scatterchart

## Enhancements 

* added argument `legend_text` to theme function
* legend can be dropped now
* `ms_linechart` now accepts a non-numeric x axis.


# mschart 0.2.1

* Fix issue that corrupted the file when data had missing values
