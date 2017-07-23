#' @title scatter object
#' @description Creation of a scatter object that can be
#' inserted in a 'Microsoft' document.
#' @param data a data.frame
#' @param x x colname
#' @param y y colname
#' @param group grouping colname used to split data into series. Optional.
#' @export
ms_scatter <- function(data, x, y, group = NULL){
  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  if( !is.null(group) && !(group %in% names(data)) ){
    stop("column ", shQuote(group), " could not be found in data.")
  }

  theme_ <- mschart_theme()
  options_ <- scatter_options()
  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")
  data_labels_options_ <- data_labels_options()
  lbls <- list(title = NULL, x = x, y = y)

  out <- list(data = data, x = x, y = y, group = group,
              theme = theme_, options = options_,
              x_axis = x_axis_, y_axis = y_axis_,
              data_labels_options = data_labels_options_,
              labels = lbls)
  class(out) <- "ms_scatter"
  out

}

#' @export
set_scatter_options <- function( x, opts ){
  x$options <- opts
  x
}

ooml_chart  <- function(x, data_file){
  if( inherits(x, "ms_barchart" ) ){
    ooml_barchart(x, data_file)
  } else if( inherits(x, "ms_scatter" ) ){
    ooml_scatter(x, data_file)
  } else {
    stop("unimplemented")
  }
}



#' @importFrom purrr map_chr
ooml_scatter <- function(x, data_file){

  write.xlsx(x$data[, c(x$x, x$y, x$group) ], file = data_file, sheetName = "sheet1")

  series <- as_scatter_series(x)

  str_series_ <- map_chr(series, function(x) x$pml() )
  str_series_ <- paste(str_series_, collapse = "")

  str_ <- paste0( "<c:scatterChart>",
                  sprintf("<c:scatterStyle val=\"%s\"/>", x$options$scatter_style),
                  sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
                  str_series_,
                  pml_labels_options(x$data_labels_options),
                  "<c:axId val=\"64451712\"/>",
                  "<c:axId val=\"64453248\"/>",
                  "</c:scatterChart>"  )

  x_axis_str <- pml_axis_options( x$x_axis, id = "64451712", theme = x$theme,
                                  cross_id = "64453248", is_x = TRUE, lab = x$labels$x, vertical_align = "horz")
  x_axis_str <- sprintf("<c:valAx>%s</c:valAx>", x_axis_str)

  y_axis_str <- pml_axis_options( x$y_axis, id = "64453248", theme = x$theme,
                                  cross_id = "64451712", is_x = FALSE, lab = x$labels$y, vertical_align = "horz")
  y_axis_str <- sprintf("<c:valAx>%s</c:valAx>", y_axis_str)

  str_ <- paste0("<c:plotArea ",
                 "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
                 "<c:layout/>",str_, x_axis_str, y_axis_str, "</c:plotArea>")

  str_
}

#' @importFrom data.table as.data.table dcast.data.table setorderv
get_scatter_data <- function(x){
  dataset <- x$data

  if( !is.null(x$group)){
    split(dataset[, c(x$x, x$y)], dataset[[x$group]])
  } else list(dataset[, c(x$x, x$y)])
}


#' @importFrom cellranger cell_limits as.range ra_ref to_string
as_scatter_series <- function(x){

  dataset <- get_scatter_data(x)
  cum_size_end <- cumsum(sapply(dataset, nrow))
  cum_size_start <- 1+c(0, cum_size_end[-length(cum_size_end)])
  cum_size_start <- as.integer(cum_size_start) + 1
  cum_size_end <- as.integer(cum_size_end) + 1
  series <- list()

  for( gr_ in seq_along(dataset)){
    w_x <- which( names(dataset[[gr_]]) %in% x$x )
    w_y <- which( names(dataset[[gr_]]) %in% x$y )

    serie_x_range <- cell_limits(ul = c(cum_size_start[gr_], w_x),
                                 lr = c(cum_size_end[gr_], w_x),
                                 sheet = "sheet1")
    serie_x_range <- as.range(serie_x_range, fo = "A1", strict = TRUE, sheet = TRUE)
    serie_x_ <- num_ref$new( serie_x_range, dataset[[gr_]][[x$x]] )


    serie_name_range <- ra_ref(row_ref = 1, col_ref = w_y, sheet = "sheet1")
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    serie_name_ref_ <- str_ref$new( serie_name_range, x$y )

    serie_value_range <- cell_limits(ul = c(cum_size_start[gr_], w_y), lr = c(cum_size_end[gr_], w_y),  sheet = "sheet1")
    serie_value_range <- as.range(serie_value_range, fo = "A1", strict = TRUE, sheet = TRUE)
    serie_values_ <- num_ref$new( serie_value_range, dataset[[gr_]][[x$y]] )
    ser <- serie_scatter$new( idx = length(series),
                           order = length(series),
                           tx = serie_name_ref_,
                           val_x = serie_x_, val_y = serie_values_, x$options$scatter_style )
    series <- append(series, list(ser) )
  }
  series

}
