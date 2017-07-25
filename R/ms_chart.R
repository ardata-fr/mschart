ms_chart <- function(data, x, y, group = NULL){

  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  if( !is.null(group) && !(group %in% names(data)) ){
    stop("column ", shQuote(group), " could not be found in data.", call. = FALSE)
  }

  theme_ <- chart_theme()

  x_axis_tag <- get_axis_tag(data[[x]])
  y_axis_tag <- get_axis_tag(data[[y]])

  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")

  label_settings_ <- data_labels_options()
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
              label_settings = label_settings_,
              labels = lbls)
  class(out) <- c("ms_chart")
  out$data_series <- shape_as_series(out)
  out
}

format.ms_chart  <- function(x, id_x, id_y){
  str_ <- ooml_code(x, id_x = id_x, id_y = id_y)

  if( is.null(x$x_axis$num_fmt) )
    x$x_axis$num_fmt <- x$theme[[x$fmt_names$x]]
  if( is.null(x$y_axis$num_fmt) )
    x$y_axis$num_fmt <- x$theme[[x$fmt_names$y]]

  axes_str <- axes_xml(x, id_x = id_x, id_y = id_y)
  ns <- "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">"
  paste0("<c:plotArea ", ns, "<c:layout/>", str_, axes_str, "</c:plotArea>")
}


#' @describeIn mschart linechart function
#' @export
ms_linechart <- function(data, x, y, group = NULL){
  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- linechart_options()
  class(out) <- c("ms_linechart", "ms_chart")
  out
}

#' @describeIn mschart barchart function
#' @export
#' @examples
#' @example examples/01_barchart.R
ms_barchart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- barchart_options()
  class(out) <- c("ms_barchart", "ms_chart")
  out
}

#' @export
set_data_label <- function( x, opts ){
  x$label_settings <- opts
  x
}

#' @export
set_mschart_theme <- function( x, value ){
  x$theme <- value
  x
}

#' @export
set_labels <- function( x, title = NULL, xlab = NULL, ylab = NULL){
  if( !is.null(title) ) x$labels[["title"]] <- title
  if( !is.null(xlab) ) x$labels[["x"]] <- xlab
  if( !is.null(ylab) ) x$labels[["y"]] <- ylab
  x
}

