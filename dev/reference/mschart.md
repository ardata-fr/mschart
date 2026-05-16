# Chart Generation for 'Microsoft Word', 'Microsoft Excel' and 'Microsoft PowerPoint' Documents

It lets R users create Microsoft Office charts from data, and then add
title, legends, and annotations to the chart object.

The graph produced is a Microsoft graph, which means that it can be
edited in your Microsoft software and that the underlying data are
available.

The package will not allow you to make the same charts as with ggplot2.
It allows only a subset of the charts possible with 'Office Chart'. The
package is often used to industrialize graphs that are then consumed and
annotated by non-R users.

The following chart types are available.

Classical charts (Office 2007+ DrawingML pipeline):

- bar charts:
  [`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md)

- line charts:
  [`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md)

- scatter plots:
  [`ms_scatterchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_scatterchart.md)

- area charts:
  [`ms_areachart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_areachart.md)

- pie and doughnut charts:
  [`ms_piechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_piechart.md)

- bubble charts:
  [`ms_bubblechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_bubblechart.md)

- radar (spider) charts:
  [`ms_radarchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_radarchart.md)

- stock charts (HLC and OHLC):
  [`ms_stockchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_stockchart.md)

chartEx charts (Office 2016+ pipeline; older viewers show a
placeholder):

- box-and-whisker:
  [`ms_boxplotchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_boxplotchart.md)

- funnel:
  [`ms_funnelchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_funnelchart.md)

- histogram:
  [`ms_histogramchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_histogramchart.md)

- pareto:
  [`ms_paretochart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_paretochart.md)

- sunburst:
  [`ms_sunburstchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_sunburstchart.md)

- treemap:
  [`ms_treemapchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_treemapchart.md)

- waterfall:
  [`ms_waterfallchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_waterfallchart.md)

Several classical chart types can be combined on a single chart, with an
optional secondary axis (y or x), using
[`ms_chart_combine()`](https://ardata-fr.github.io/mschart/dev/reference/ms_chart_combine.md).

These functions create a 'chart' object that can be customized:

- by using options specific to the chart (with
  [`chart_settings()`](https://ardata-fr.github.io/mschart/dev/reference/chart_settings.md)),

- by changing the options related to the axes (with
  [`chart_ax_x()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_x.md)
  and
  [`chart_ax_y()`](https://ardata-fr.github.io/mschart/dev/reference/chart_ax_y.md)),

- by changing the options related to the labels (with
  [`chart_data_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_labels.md)),

- by changing the colors, line widths, ... with functions

  - [`chart_labels_text()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels_text.md)

  - [`chart_data_fill()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_fill.md)

  - [`chart_data_line_style()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_line_style.md)

  - [`chart_data_line_width()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_line_width.md)

  - [`chart_data_size()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_size.md)

  - [`chart_data_smooth()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_smooth.md)

  - [`chart_data_stroke()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_stroke.md)

  - [`chart_data_symbol()`](https://ardata-fr.github.io/mschart/dev/reference/chart_data_symbol.md)

- by changing the general theme with function
  [`chart_theme()`](https://ardata-fr.github.io/mschart/dev/reference/set_theme.md),

- by changing the title labels with function
  [`chart_labels()`](https://ardata-fr.github.io/mschart/dev/reference/chart_labels.md).

You can add a chart into a slide in PowerPoint with function
[`ph_with.ms_chart()`](https://ardata-fr.github.io/mschart/dev/reference/ph_with.ms_chart.md).

You can add a chart into a Word document with function
[`body_add_chart()`](https://ardata-fr.github.io/mschart/dev/reference/body_add_chart.md).

You can add a chart into an Excel sheet with function
[`sheet_add_drawing.ms_chart()`](https://ardata-fr.github.io/mschart/dev/reference/sheet_add_drawing.ms_chart.md).

### Series styling properties by chart type

Not all series styling properties have an effect on every chart type. A
warning is emitted when a property is set on a chart type that does not
support it.

Classical charts:

|            |     |      |      |         |       |       |        |     |
|------------|-----|------|------|---------|-------|-------|--------|-----|
| Property   | bar | line | area | scatter | stock | radar | bubble | pie |
| fill       | x   | x    | x    | x       | x     | x     | x      | x   |
| colour     | x   | x    | x    | x       | x     | x     | x      | x   |
| symbol     |     | x    |      | x       | x     | x     |        |     |
| size       |     | x    |      | x       | x     | x     |        |     |
| line_width | x   | x    | x    | x       | x     | x     | x      | x   |
| line_style |     | x    |      | x       | x     | x     |        |     |
| smooth     |     | x    |      | x       |       |       |        |     |
| labels_fp  | x   | x    | x    | x       |       | x     |        | x   |

chartEx charts expose a narrower set of styling knobs, since most visual
aspects are computed by Office from the data (bins, levels, connectors,
...). For these, prefer the chart-type-specific
[`chart_settings()`](https://ardata-fr.github.io/mschart/dev/reference/chart_settings.md)
method when one is available (e.g. for pareto and boxplot:
`chart_settings(x, line = fp_border(...))`).

|           |         |        |           |        |          |         |           |
|-----------|---------|--------|-----------|--------|----------|---------|-----------|
| Property  | boxplot | funnel | histogram | pareto | sunburst | treemap | waterfall |
| fill      | x       | x      | x         | x      | x        | x       | x         |
| colour    | x       | x      | x         | x      | x        | x       | x         |
| labels_fp | x       | x      | x         | x      | x        | x       | x         |

### Two arguments often confused: `asis` and `write_data`

`asis` is a **constructor argument** (on
[`ms_barchart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_barchart.md),
[`ms_linechart()`](https://ardata-fr.github.io/mschart/dev/reference/ms_linechart.md)
and most other classical constructors). It describes the *input shape*
of the data frame:

- `asis = FALSE` (default): long format, with a `group` column that
  splits the rows into series. `mschart` reshapes the data internally.

- `asis = TRUE`: wide format, with one column per series. `y` accepts a
  vector of series column names.

`write_data` is an **embed-time argument** of
[`sheet_add_drawing.ms_chart()`](https://ardata-fr.github.io/mschart/dev/reference/sheet_add_drawing.ms_chart.md)
for the Excel pipeline. It decides whether `mschart` writes the chart's
data into the target sheet (`TRUE`, the default) or leaves you in charge
of placing it via
[`officer::sheet_write_data()`](https://davidgohel.github.io/officer/reference/sheet_write_data.html)
(`FALSE`, recommended for non-trivial workbooks).

The two arguments are independent: a chart built with `asis = TRUE` can
still be embedded with either `write_data = TRUE` or
`write_data = FALSE`.

## See also

<https://ardata-fr.github.io/officeverse/>

## Author

**Maintainer**: David Gohel <david.gohel@ardata.fr>

Authors:

- David Gohel <david.gohel@ardata.fr>

Other contributors:

- ArData \[copyright holder\]

- YouGov \[funder\]

- Jan Marvin Garbuszus (support for openxlsx2 and combo charts)
  \[contributor\]

- Stefan Moog <moogs@gmx.de> (support to set chart and plot area color
  and border, fix for line style none) \[contributor\]

- Eli Daniels <eli.daniels@ardata.fr> \[contributor\]

- Marlon Molina (added table feature) \[contributor\]

- Rokas Klydzia (custom labels) \[contributor\]

- David Camposeco <david.camposeco.paulsen@gmail.com> (chart_data_smooth
  function) \[contributor\]

- Dan Joplin (fix scatter plot data structure) \[contributor\]
