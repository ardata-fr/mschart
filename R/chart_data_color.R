#' @export
#' @title Modify fill colour
#' @description Specify mappings from levels in the data to displayed fill colours.
#' @param x an \code{ms_chart} object.
#' @param values `character(num of series|1)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one colour, this colour will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' @seealso \code{\link{chart_data_stroke}}, \code{\link{chart_data_symbol}}, \code{\link{chart_data_size}}
chart_data_fill <- function(x, values){

  valid_cols <- is_valid_color(values)
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$fill)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$fill[names(values)] <- values
  x
}

#' @export
#' @title Modify marker stroke colour
#' @description Specify mappings from levels in the data to displayed marker stroke colours.
#' @param x an \code{ms_chart} object.
#' @param values `character(num of series)`: a set of colours values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one colour, this colour will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' @seealso \code{\link{chart_data_fill}}, \code{\link{chart_data_symbol}}, \code{\link{chart_data_size}}
chart_data_stroke <- function(x, values){

  valid_cols <- is_valid_color(values)
  if( any(!valid_cols) )
    stop("invalid color(s) in argument values")

  serie_names <- names(x$series_settings$colour)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))


  x$series_settings$colour[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol
#' @description Specify mappings from levels in the data to displayed symbols.
#' @param x an \code{ms_chart} object.
#' @param values `character(num of series)`: a set of symbol values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' Possible values are: 'circle', 'dash', 'diamond', 'dot', 'none', 'plus',
#' 'square', 'star', 'triangle', 'x', 'auto'.
#' If it contains only one symbol, this symbol will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' @seealso \code{\link{chart_data_fill}}, \code{\link{chart_data_stroke}}, \code{\link{chart_data_size}}
chart_data_symbol <- function(x, values){

  if( !all(values %in% st_markerstyle) ){
    stop("values should have values matching ", paste0(shQuote(st_markerstyle), collapse = ", " ))
  }

  serie_names <- names(x$series_settings$symbol)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))


  x$series_settings$symbol[names(values)] <- values
  x
}

#' @export
#' @title Modify symbol size
#' @description Specify mappings from levels in the data to displayed size of symbols.
#' @param x an \code{ms_chart} object.
#' @param values `double(num of series)`: a set of size values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one size, this size will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#' @seealso \code{\link{chart_data_fill}}, \code{\link{chart_data_stroke}}, \code{\link{chart_data_symbol}}
chart_data_size <- function(x, values){

  if( !is.numeric(values) )
    stop("values should be numeric values")
  if( any( sign(values) < 0 ) )
    stop("values should not contain negative values")

  serie_names <- names(x$series_settings$size)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$size[names(values)] <- values
  x
}

#' @export
#' @title Modify line width
#' @description Specify mappings from levels in the data to displayed line width between symbols.
#' @param x an \code{ms_chart} object.
#' @param values `double(num of series)`: a set of size values to map data values to.
#' It is a named vector, the values will be matched based on the names.
#' If it contains only one size, this size will be associated to all existing series.
#' @examples
#' my_scatter <- ms_scatterchart(data = iris, x = "Sepal.Length",
#'   y = "Sepal.Width",  group = "Species")
#' my_scatter <- chart_settings(my_scatter, scatterstyle = "lineMarker")
#' my_scatter <- chart_data_fill(my_scatter,
#'   values = c(virginica = "#6FA2FF", versicolor = "#FF6161", setosa = "#81FF5B") )
#' my_scatter <- chart_data_stroke(my_scatter,
#'   values = c(virginica = "black", versicolor = "black", setosa = "black") )
#' my_scatter <- chart_data_symbol(my_scatter,
#'   values = c(virginica = "circle", versicolor = "diamond", setosa = "circle") )
#' my_scatter <- chart_data_size(my_scatter,
#'   values = c(virginica = 20, versicolor = 16, setosa = 20) )
#' my_scatter <- chart_data_line_width(my_scatter,
#'   values = c(virginica = 2, versicolor = 3, setosa = 6) )
#' @seealso \code{\link{chart_data_fill}}, \code{\link{chart_data_stroke}}, \code{\link{chart_data_symbol}}
chart_data_line_width <- function(x, values){

  if( !is.numeric(values) )
    stop("values should be numeric values")
  if( any( sign(values) < 0 ) )
    stop("values should not contain negative values")

  serie_names <- names(x$series_settings$line_width)

  if( length(values) == 1 ){
    values <- rep(values, length(serie_names))
    names(values) <- serie_names
  }

  if( !all(names(values) %in% serie_names ) )
    stop( "values's names do not match series' names: ", paste0(shQuote(serie_names), collapse = ", "))

  x$series_settings$line_width[names(values)] <- values
  x
}

