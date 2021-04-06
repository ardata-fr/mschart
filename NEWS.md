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
