ms_chart <- function(data, x, y, group = NULL){

  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  if( !is.null(group) && !(group %in% names(data)) ){
    stop("column ", shQuote(group), " could not be found in data.", call. = FALSE)
  }

  theme_ <- mschart_theme()

  x_axis_tag <- get_axis_tag(data[[x]])
  y_axis_tag <- get_axis_tag(data[[y]])

  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")


  lbls <- list(title = NULL, x = x, y = y)

  out <- list(data = data, x = x, y = y, group = group,
              theme = theme_,
              options = list(),
              x_axis = x_axis_,
              y_axis = y_axis_,
              axis_tag = list(x = x_axis_tag,
                              y = y_axis_tag),
              fmt_names = list( x = fmt_name(data[[x]]),
                                y = fmt_name(data[[y]]) ),
              labels = lbls)
  class(out) <- c("ms_chart")
  out <- chart_data_labels(out)
  out$data_series <- shape_as_series(out)

  series_names <- setdiff(colnames( out$data_series ), x )
  series_fills <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
                    "#A6761D", "#666666")[seq_along(series_names)]
  series_colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
                      "#A65628", "#F781BF")[seq_along(series_names)]
  series_symbols <- rep("circle", length(series_names) )
  series_size <- rep(12, length(series_names) )
  out$series_settings <- list( fill = setNames(series_fills, series_names),
        colour = setNames(series_colours, series_names),
        symbol = setNames(series_symbols, series_names),
        size = setNames(series_size, series_names)
        )
  out
}

#' @importFrom xml2 xml_attr<-
format.ms_chart  <- function(x, id_x, id_y){
  str_ <- ooml_code(x, id_x = id_x, id_y = id_y)


  if( is.null(x$x_axis$num_fmt) )
    x$x_axis$num_fmt <- x$theme[[x$fmt_names$x]]
  if( is.null(x$y_axis$num_fmt) )
    x$y_axis$num_fmt <- x$theme[[x$fmt_names$y]]

  axes_str <- axes_xml(x, id_x = id_x, id_y = id_y)
  ns <- "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
  xml_elt <- paste0("<c:plotArea ", ns, "<c:layout/>", str_, axes_str, "</c:plotArea>")

  xml_doc <- read_xml(system.file(package = "officer", "template", "chart.xml"))

  node <- xml_find_first(xml_doc, "//c:plotArea")
  xml_replace( node, as_xml_document(xml_elt) )

  if( !is.null( x$labels[["title"]] ) ){
    chartnode <- xml_find_first(xml_doc, "//c:chart")
    title_ <- "<c:title %s><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, ns, format(x$theme[["main_title"]], type = "pml" ), x$labels[["title"]] )
    xml_add_child( chartnode, as_xml_document(title_), .where	= 0 )
  }
  legend_pos <- xml_find_first(xml_doc, "//c:chart/c:legend/c:legendPos")
  xml_attr( legend_pos, "val" ) <- x$theme[["legend_position"]]

  x$theme[["main_title"]]
  if( !is.null( x$labels[["title"]] ) ){
    chartnode <- xml_find_first(xml_doc, "//c:chart")
    title_ <- "<c:title %s><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, ns, format(x$theme[["main_title"]], type = "pml" ), x$labels[["title"]] )
    xml_add_child( chartnode, as_xml_document(title_), .where	= 0 )
  }

  as.character(xml_doc)
}


#' @describeIn mschart line plot
#' @export
#' @examples
#' library(officer)
#'
#'
#' ##########################
#' # linecharts example -----
#' ##########################
#'
#'
#' @example examples/02_linechart.R
ms_linechart <- function(data, x, y, group = NULL){
  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- linechart_options()
  class(out) <- c("ms_linechart", "ms_chart")
  out
}

#' @describeIn mschart bar plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # barcharts example -----
#' ##########################
#'
#'
#' @example examples/01_barchart.R
ms_barchart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- barchart_options()
  class(out) <- c("ms_barchart", "ms_chart")
  out
}

#' @describeIn mschart area plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # areacharts example -----
#' ##########################
#'
#'
#' @example examples/03_areachart.R
ms_areachart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  class(out) <- c("ms_areachart", "ms_chart")
  out <- chart_settings(out)
  out
}

#' @describeIn mschart scatter plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # scattercharts example -----
#' ##########################
#'
#'
#' @example examples/04_scatterchart.R
ms_scatterchart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  class(out) <- c("ms_scatterchart", "ms_chart")
  out <- chart_settings(out)
  out
}

#' @export
#' @title Modify axis and plot labels
#' @description Add labels to a chart, labels can be specified for
#' x axis, y axis and plot.
#' @param x chart object
#' @param title,xlab,ylab Text to add
#' @examples
#' mylc <- ms_linechart(data = browser_ts, x = "date",
#'   y = "freq", group = "browser")
#' mylc <- chart_labels(mylc, title = "my title", xlab = "my x label",
#'   ylab = "my y label")
chart_labels <- function( x, title = NULL, xlab = NULL, ylab = NULL){
  if( !is.null(title) ) x$labels[["title"]] <- title
  if( !is.null(xlab) ) x$labels[["x"]] <- xlab
  if( !is.null(ylab) ) x$labels[["y"]] <- ylab
  x
}

