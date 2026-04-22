# mschart 0.4.2

## New features

* New function `ms_bubblechart()` for bubble charts. A bubble chart
is a scatter chart where each point has a third numeric dimension
controlling its size.
* New function `ms_radarchart()` for radar (spider) charts. 
* New function `ms_stockchart()` for stock charts. Supports
High-Low-Close (HLC) and Open-High-Low-Close (OHLC) modes.
OHLC charts display candlestick up/down bars. Use `chart_settings()`
to customise high-low lines and up/down bar appearance.
* New method `sheet_add_drawing.ms_chart()` to add charts to Excel
sheets via `officer::sheet_add_drawing()`. Chart data is written
into the sheet and referenced by the chart.
* New function `ms_chart_combine()` to combine multiple chart types
(e.g. bars + lines) into a single chart with optional secondary axes.
Charts are passed as named arguments; use `secondary_y` to assign
charts to the right-hand axis.
* New function `ms_piechart()` for pie and doughnut charts. Use
`chart_settings(x, hole_size = ...)` to control the hole size
(0 = pie, >0 = doughnut). Contributed by Jan Marvin Garbuszus.
* `chart_ax_x()` and `chart_ax_y()` gain a `second_axis` argument
for use with `ms_chart_combine()`.
* `chart_ax_x()` and `chart_ax_y()` gain `major_unit`, `minor_unit`,
`major_time_unit` and `minor_time_unit` parameters to control axis
interval spacing (#105).
* `chart_settings.ms_stockchart()` gains a `table` argument to display
the data table below a stock chart, matching the behaviour already
available on bar / line / area charts.
* `chart_settings.ms_linechart()` gains a `grouping` argument. Line
charts can now be stacked or percent-stacked (allowed values:
`"standard"`, `"stacked"`, `"percentStacked"`; `"clustered"` remains
bar-only).
* `chart_data_fill()` gains an `update_stroke` argument, default
`TRUE`. When the series stroke is not `"transparent"`, the stroke
colour is now updated together with the fill, so a single call
produces a filled shape and a matching border. Pass
`update_stroke = FALSE` to keep the current stroke colours untouched
and manage them independently via `chart_data_stroke()`. Series
whose stroke was set to `"transparent"` by the constructor
(`ms_areachart()`, `ms_piechart()`) are preserved as is.
* Series styling functions (`chart_data_fill()`, `chart_data_stroke()`,
`chart_data_symbol()`, `chart_data_size()`, `chart_data_line_width()`,
`chart_data_line_style()`, `chart_data_smooth()`, `chart_labels_text()`)
now emit a warning when applied to a chart type that does not support
the property. The supported matrix is documented in `?mschart`.
  
## Issues

* `ms_scatterchart` no longer silently drops the line when `line_width`
is below 1pt; thin lines (e.g. 0.25, 0.5, 0.75pt) are now rendered
consistently with other chart types.
* Fixed an example in `?chart_data_size` that passed a non-existent
`scatterstyle` argument to `chart_settings()` (the parameter name is
`style`).
* Grid lines can now be disabled by setting `grid_major_line_x`,
`grid_major_line_y`, `grid_minor_line_x` or `grid_minor_line_y`
to `FALSE` in `mschart_theme()` or `chart_theme()`.

## Issues

* Fixed `chart_data_line_style()` not hiding lines when style is set
to `"none"`. The generated XML now explicitly uses `<a:noFill/>` instead
of omitting the line element, which caused Excel to apply the default
style. Reported in #91, contributed by Stefan Moog (#99).
* Fixed `fmt_name()` returning the input data instead of the format
name string. Automatic axis number formatting from theme (e.g.
`date_fmt`, `double_fmt`) now works correctly.
* Fixed `chart_data_smooth()` using `symbol` series names instead of
`smooth` series names to resolve series.
* Fixed `chart_settings()` for linechart, areachart and scatterchart
silently resetting previously set options when called with partial
parameters. All methods now use the `if(missing())` pattern to
preserve existing option values.


## Changes

* Internal options field `linestyle` (linechart) and `scatterstyle`
(scatterchart, bubblechart) have been unified to `style`, matching
the user-facing `chart_settings()` parameter name. No user-visible
change; direct access to `x$options$linestyle` or
`x$options$scatterstyle` will no longer work.
* `chart_settings.ms_bubblechart()` no longer accepts a `style`
argument. It was validated and stored but never read during XML
generation (bubble charts have no `<c:scatterStyle>` counterpart in
OOXML), so any value was silently ignored. Passing `style = ...` to
`chart_settings()` on a bubble chart now errors out instead of
silently doing nothing.
* Minimum R version bumped to 3.5, officer to 0.6.7.
* Fixed typos in internal code (`asssert_scatter`, `unknow`).
* Removed deprecated roxygen tags (`@docType`, `@keywords datasets`).
* Updated GitHub Actions to v4.
* switch from tinytest to testthat

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
