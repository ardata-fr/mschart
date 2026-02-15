#' @title Chart Generation for 'Microsoft Word' and 'Microsoft PowerPoint' Documents
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
#' * barcharts: [ms_barchart()]
#' * line charts: [ms_linechart()]
#' * scatter plots: [ms_scatterchart()]
#' * area charts: [ms_areachart()]
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
#'
#' @seealso \url{https://ardata-fr.github.io/officeverse/}
#' @name mschart
"_PACKAGE"
