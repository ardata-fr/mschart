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
#' The following chart types are available:
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
#' Several chart types can be combined on a single chart, with an
#' optional secondary axis, using [ms_chart_combine()].
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
#' chart type. The following table shows which properties are
#' supported. A warning is emitted when a property is set on
#' a chart type that does not support it.
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
#' @seealso \url{https://ardata-fr.github.io/officeverse/}
#' @name mschart
"_PACKAGE"
