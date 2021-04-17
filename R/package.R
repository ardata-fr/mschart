#' mschart: Chart Generation for 'Microsoft Word' and 'Microsoft PowerPoint' Documents
#'
#' It lets R users to create Microsoft Office charts from data, and
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
#' The following charts are the only available from all possible MS charts:
#'
#' * barcharts: [ms_barchart()]
#' * line charts: [ms_linechart()]
#' * scatter plots: [ms_scatterchart()]
#' * area charts: [ms_areachart()]
#'
#' @seealso \url{https://ardata-fr.github.io/officeverse/}
#' @docType package
#' @name mschart
"_PACKAGE"
