#' @title Chart Generation for 'Microsoft Word', 'Microsoft Excel' and 'Microsoft PowerPoint' Documents
#'
#' @description It lets R users create Microsoft Office charts from data, and
#' then add title, legends, and annotations to the chart object.
#'
#' The graph produced is a Microsoft graph, which means that it can be edited in
#' your Microsoft software and that the underlying data are available.
#'
#' The package will not allow you to make the same charts as with ggplot2. It
#' allows only a subset of the charts possible with 'Office Chart'. The package is
#' often used to industrialize graphs that are then consumed and annotated by non-R
#' users.
#'
#' The following chart types are available.
#'
#' Classical charts (Office 2007+ DrawingML pipeline):
#'
#' * bar charts: [ms_barchart()]
#' * line charts: [ms_linechart()]
#' * scatter plots: [ms_scatterchart()]
#' * area charts: [ms_areachart()]
#' * pie and doughnut charts: [ms_piechart()]
#' * bubble charts: [ms_bubblechart()]
#' * radar (spider) charts: [ms_radarchart()]
#' * stock charts (HLC and OHLC): [ms_stockchart()]
#'
#' chartEx charts (Office 2016+ pipeline; older viewers show a
#' placeholder):
#'
#' * box-and-whisker: [ms_boxplotchart()]
#' * funnel: [ms_funnelchart()]
#' * histogram: [ms_histogramchart()]
#' * pareto: [ms_paretochart()]
#' * sunburst: [ms_sunburstchart()]
#' * treemap: [ms_treemapchart()]
#' * waterfall: [ms_waterfallchart()]
#'
#' Several classical chart types can be combined on a single chart,
#' with an optional secondary axis (y or x), using [ms_chart_combine()].
#'
#' These functions create a 'chart' object that can be customized:
#'
#' * by using options specific to the chart (with [chart_settings()]),
#' * by changing the options related to the axes (with [chart_ax_x()] and [chart_ax_y()]),
#' * by changing the options related to the labels (with [chart_data_labels()]),
#' * by changing the colors, line widths, ... with functions
#'   * [chart_labels_text()]
#'   * [chart_data_fill()]
#'   * [chart_data_line_style()]
#'   * [chart_data_line_width()]
#'   * [chart_data_size()]
#'   * [chart_data_smooth()]
#'   * [chart_data_stroke()]
#'   * [chart_data_symbol()]
#' * by changing the general theme with function [chart_theme()],
#' * by changing the title labels with function [chart_labels()].
#'
#' You can add a chart into a slide in PowerPoint with function [ph_with.ms_chart()].
#'
#' You can add a chart into a Word document with function [body_add_chart()].
#'
#' You can add a chart into an Excel sheet with function [sheet_add_drawing.ms_chart()].
#'
#'
#' ## Series styling properties by chart type
#'
#' Not all series styling properties have an effect on every
#' chart type. A warning is emitted when a property is set on
#' a chart type that does not support it.
#'
#' Classical charts:
#'
#' | Property | bar | line | area | scatter | stock | radar | bubble | pie |
#' |:-----------|:---:|:----:|:----:|:-------:|:-----:|:-----:|:------:|:---:|
#' | fill       | x   | x    | x    | x       | x     | x     | x      | x   |
#' | colour     | x   | x    | x    | x       | x     | x     | x      | x   |
#' | symbol     |     | x    |      | x       | x     | x     |        |     |
#' | size       |     | x    |      | x       | x     | x     |        |     |
#' | line_width | x   | x    | x    | x       | x     | x     | x      | x   |
#' | line_style |     | x    |      | x       | x     | x     |        |     |
#' | smooth     |     | x    |      | x       |       |       |        |     |
#' | labels_fp  | x   | x    | x    | x       |       | x     |        | x   |
#'
#' chartEx charts expose a narrower set of styling knobs, since most
#' visual aspects are computed by Office from the data (bins, levels,
#' connectors, ...). For these, prefer the chart-type-specific
#' `chart_settings()` method when one is available (e.g. for pareto
#' and boxplot: `chart_settings(x, line = fp_border(...))`).
#'
#' | Property | boxplot | funnel | histogram | pareto | sunburst | treemap | waterfall |
#' |:-----------|:-------:|:------:|:---------:|:------:|:--------:|:-------:|:---------:|
#' | fill       | x       | x      | x         | x      | x        | x       | x         |
#' | colour     | x       | x      | x         | x      | x        | x       | x         |
#' | labels_fp  | x       | x      | x         | x      | x        | x       | x         |
#'
#' ## Two arguments often confused: `asis` and `write_data`
#'
#' `asis` is a **constructor argument** (on [ms_barchart()],
#' [ms_linechart()] and most other classical constructors). It
#' describes the *input shape* of the data frame:
#'
#' * `asis = FALSE` (default): long format, with a `group` column that
#'   splits the rows into series. `mschart` reshapes the data
#'   internally.
#' * `asis = TRUE`: wide format, with one column per series. `y`
#'   accepts a vector of series column names.
#'
#' `write_data` is an **embed-time argument** of
#' [sheet_add_drawing.ms_chart()] for the Excel pipeline. It decides
#' whether `mschart` writes the chart's data into the target sheet
#' (`TRUE`, the default) or leaves you in charge of placing it via
#' [officer::sheet_write_data()] (`FALSE`, recommended for non-trivial
#' workbooks).
#'
#' The two arguments are independent: a chart built with `asis = TRUE`
#' can still be embedded with either `write_data = TRUE` or
#' `write_data = FALSE`.
#'
#' @seealso \url{https://ardata-fr.github.io/officeverse/}
#' @name mschart
"_PACKAGE"
