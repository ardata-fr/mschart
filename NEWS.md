# mschart 0.4.1

## Issues

* fix issue with dcast by making sure all data are preserved.
* Fix. Account for "none" linestyle set via `chart_data_line_style`. Closes #91.

## New features

* Add support to set chart and plot area color and border by Stefan Moog ;
added four new theme arguments chart/plot_background and chart/plot_border, 
new theme `theme_ggplot2()`.



# mschart 0.4.0

## New features

* Support for openxlsx2 by Jan Marvin Garbuszus
* option to add table of data below corresponding levels by Marlon Molina

## Issues

* fix issue with % in labels of the graphic
* stop reordering data when a group is used, user is expected to 
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
* `ms_linechart` now accept non numeric x axis.


# mschart 0.2.1

* Fix issue that made file *corrupted* when data has missing values
