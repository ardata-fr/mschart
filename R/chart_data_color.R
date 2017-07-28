#' @export
#' @title Modify fill colour
#' @description Modify fill colour for series
#' @param x chart object
#' @param values `character(num of series)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
chart_data_fill <- function(x, values){
  x$series_settings$fill[names(values)] <- values
  x
}

#' @export
#' @title Modify stroke colour
#' @description Modify stroke colour for series
#' @param x chart object
#' @param values `character(num of series)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
chart_data_stroke <- function(x, values){

  x$series_settings$colour[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol
#' @description Modify symbol for series
#' @param x chart object
#' @param values `character(num of series)`: a set of symbol values to map data values to.
#' It is a named vector, the values will be matched based on the names.
chart_data_symbol <- function(x, values){

  if( !all(values %in% st_markerstyle) ){
    stop("values should have values matching ", paste0(shQuote(st_markerstyle), collapse = ", " ))
  }
  x$series_settings$symbol[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol
#' @description Modify symbol for series
#' @param x chart object
#' @param values `double(num of series)`: a set of size values to map data values to.
#' It is a named vector, the values will be matched based on the names.
chart_data_size <- function(x, values){
  x$series_settings$size[names(values)] <- values
  x
}

