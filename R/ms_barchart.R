#' @title barchart object
#' @description Creation of a barchart object that can be
#' inserted in a 'Microsoft' document.
#' @param data a data.frame
#' @param x x colname
#' @param y y colname
#' @param group grouping colname used to split data into series. Optional.
#' @export
ms_barchart <- function(data, x, y, group = NULL){
  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  if( !is.null(group) && !(group %in% names(data)) ){
    stop("column ", shQuote(group), " could not be found in data.")
  }

  theme_ <- mschart_theme()
  options_ <- barchart_options()
  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")
  data_labels_options_ <- data_labels_options()
  lbls <- list(title = NULL, x = x, y = y)

  out <- list(data = data, x = x, y = y, group = group,
              theme = theme_, options = options_,
              x_axis = x_axis_, y_axis = y_axis_,
              data_labels_options = data_labels_options_,
              labels = lbls)
  class(out) <- "ms_barchart"
  out

}

#' @export
set_bar_options <- function( x, opts ){
  x$options <- opts
  x
}

#' @export
set_data_label <- function( x, opts ){
  x$data_labels_options <- opts
  x
}

#' @export
set_x_axis <- function( x, options = axis_options(axis_position = "b") ){
  x$x_axis <- options
  x
}

#' @export
set_y_axis <- function( x, options = axis_options(axis_position = "l") ){
  x$y_axis <- options
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



#' @importFrom purrr map_chr
ooml_chart <- function(x, ...){

  series <- as_series(x)
  str_series_ <- map_chr(series, function(x) x$pml() )
  str_series_ <- paste(str_series_, collapse = "")

  str_ <- paste0( "<c:barChart>",
                  "<c:barDir val=\"%s\"/>",
                  "<c:grouping val=\"%s\"/>",
                  "<c:varyColors val=\"%.0f\"/>",
                  str_series_,
                  pml_labels_options(x$data_labels_options),
                  "<c:gapWidth val=\"%.0f\"/>",
                  "<c:overlap val=\"%.0f\"/>",
                  "<c:axId val=\"64451712\"/>",
                  "<c:axId val=\"64453248\"/>",
                  "</c:barChart>"  )

  str_ <- sprintf(str_, x$options$dir, x$options$grouping,
                  x$options$vary_colors,
                  x$options$gap_width, x$options$overlap)

  x_axis_str <- pml_axis_options( x$x_axis, id = "64451712", theme = x$theme,
                                  cross_id = "64453248", is_x = TRUE, lab = x$labels$x, vertical_align = "horz")
  x_axis_str <- sprintf("<c:catAx>%s</c:catAx>", x_axis_str)

  y_axis_str <- pml_axis_options( x$y_axis, id = "64453248", theme = x$theme,
                                  cross_id = "64451712", is_x = FALSE, lab = x$labels$y, vertical_align = "horz")
  y_axis_str <- sprintf("<c:valAx>%s</c:valAx>", y_axis_str)

  str_ <- paste0("<c:plotArea ",
                 "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
                 "<c:layout/>",str_, x_axis_str, y_axis_str, "</c:plotArea>")

  str_
}


#' @importFrom data.table as.data.table dcast.data.table setorderv
get_data <- function(x){
  dataset <- as.data.table(x$data)
  dataset <- dataset[, c(x$x, x$y, x$group), with=FALSE]
  dataset <- setorderv(dataset, c(x$x, x$group) )

  if( !is.null(x$group)){
    form_str <- sprintf("%s ~ %s", x$x, x$group)
    dataset <- dcast.data.table(dataset, as.formula(form_str), fun.aggregate = I, fill = NA )
  }
  as.data.frame(dataset)
}

#' @importFrom cellranger cell_limits as.range ra_ref to_string
as_series <- function(x){

  dataset <- get_data(x)

  w_cat <- which( names(dataset) %in% x$x )
  serie_cat_range <- cell_limits(ul = c(2, w_cat),
                                 lr = c(nrow(dataset)+1, w_cat),
                                 sheet = "sheet1")
  serie_cat_range <- as.range(serie_cat_range, fo = "A1", strict = TRUE, sheet = TRUE)
  serie_cat_ <- str_ref$new( serie_cat_range, dataset[[x$x]] )

  series <- list()
  w_vals <- which( names(dataset) %in% setdiff(names(dataset), x$x) )

  for( w_val in w_vals){
    val_col <- names(dataset)[w_val]
    serie_name_range <- ra_ref(row_ref = 1, col_ref = w_val, sheet = "sheet1")
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    serie_name_ref_ <- str_ref$new( serie_name_range, val_col )

    serie_value_range <- cell_limits(ul = c(2, w_val), lr = c(nrow(dataset)+1, w_val),  sheet = "sheet1")
    serie_value_range <- as.range(serie_value_range, fo = "A1", strict = TRUE, sheet = TRUE)
    serie_values_ <- num_ref$new( serie_value_range, dataset[[val_col]] )


    ser <- serie_data$new( idx = length(series),
                           order = length(series),
                           tx = serie_name_ref_,
                           cat = serie_cat_, val = serie_values_ )
    series <- append(series, list(ser) )
  }
  series

}
