#' @title set chart theme
#' @description Modify chart theme with function \code{set_theme}.
#' @param x chart object.
#' @param value a \code{mschart_theme} object.
#' @examples
#' library(officer)
#' mytheme <- mschart_theme(
#'   axis_title = fp_text(color = "red", font.size = 24, bold = TRUE),
#'   grid_major_line_y = fp_border(width = 1, color = "orange"),
#'   axis_ticks_y = fp_border(width = .4, color = "gray") )
#'
#'
#' my_bc <- ms_barchart(data = browser_data, x = "browser",
#'                               y = "value", group = "serie")
#' my_bc <- chart_settings( my_bc, dir="bar", grouping="stacked",
#'                                   gap_width = 150, overlap = 100 )
#' my_bc <- set_theme(my_bc, mytheme)
#'
#'
#'
#' my_bc_2 <- ms_barchart(data = browser_data, x = "browser",
#'                               y = "value", group = "serie")
#' my_bc_2 <- chart_theme(my_bc_2,
#'   grid_major_line_y = fp_border(width = .5, color = "cyan") )
#' @export
set_theme <- function( x, value ){
  x$theme <- value
  x
}


#' @importFrom officer fp_text fp_border
#' @description Use \code{mschart_theme()} to create a chart theme.
#' @param axis_title,axis_title_x,axis_title_y axis title formatting properties (\link[officer]{fp_text})
#' @param main_title title formatting properties (\link[officer]{fp_text})
#' @param axis_text,axis_text_x,axis_text_y axis text formatting properties (\link[officer]{fp_text})
#' @param axis_ticks,axis_ticks_x,axis_ticks_y axis ticks formatting properties (\link[officer]{fp_border})
#' @param grid_major_line,grid_major_line_x,grid_major_line_y major grid lines formatting properties (\link[officer]{fp_border})
#' @param grid_minor_line,grid_minor_line_x,grid_minor_line_y minor grid lines formatting properties (\link[officer]{fp_border})
#' @param date_fmt date format
#' @param str_fmt string or factor format
#' @param double_fmt double format
#' @param integer_fmt integer format
#' @param legend_position `character(1)`: it specifies the position of the legend. It should be
#' one of \Sexpr[stage=render, results=rd]{mschart:::choices_rd(mschart:::st_legendpos)}
#' @rdname set_theme
#' @export
mschart_theme <- function(axis_title = fp_text(bold = TRUE, font.size = 16), axis_title_x = axis_title, axis_title_y = axis_title,
                          main_title = fp_text(bold = TRUE, font.size = 20),
                          axis_text = fp_text(), axis_text_x = axis_text, axis_text_y = axis_text,
                          axis_ticks = fp_border(color = "#99999999"), axis_ticks_x = axis_ticks, axis_ticks_y = axis_ticks,
                          grid_major_line = fp_border(color = "#99999999", style = "dashed"), grid_major_line_x = grid_major_line, grid_major_line_y = grid_major_line,
                          grid_minor_line = fp_border(width = 0), grid_minor_line_x = grid_minor_line, grid_minor_line_y = grid_minor_line,
                          date_fmt = "yyyy/mm/dd", str_fmt = "General", double_fmt = "#,##0.00", integer_fmt = "0", legend_position = "b" ){

  stopifnot(inherits(main_title, "fp_text"))
  stopifnot(inherits(axis_title, "fp_text"))
  stopifnot(inherits(axis_title_x, "fp_text"))
  stopifnot(inherits(axis_title_y, "fp_text"))
  stopifnot(inherits(axis_text, "fp_text"))
  stopifnot(inherits(axis_text_x, "fp_text"))
  stopifnot(inherits(axis_text_y, "fp_text"))
  stopifnot(inherits(axis_ticks, "fp_border"))
  stopifnot(inherits(axis_ticks_x, "fp_border"))
  stopifnot(inherits(axis_ticks_y, "fp_border"))
  stopifnot(inherits(grid_major_line, "fp_border"))
  stopifnot(inherits(grid_major_line_x, "fp_border"))
  stopifnot(inherits(grid_major_line_y, "fp_border"))
  stopifnot(inherits(grid_minor_line, "fp_border"))
  stopifnot(inherits(grid_minor_line_x, "fp_border"))
  stopifnot(inherits(grid_minor_line_y, "fp_border"))

  if( !legend_position %in% st_legendpos ){
    stop("legend_position should be one of ", paste0(shQuote(st_legendpos), collapse = ", " ))
  }

  out <- list(main_title = main_title, axis_title_x = axis_title_x, axis_title_y = axis_title_y,
              axis_text_x = axis_text_x, axis_text_y = axis_text_y,
              axis_ticks_x = axis_ticks_x, axis_ticks_y = axis_ticks_y,
              grid_major_line_x = grid_major_line_x, grid_major_line_y = grid_major_line_y,
              grid_minor_line_x = grid_minor_line_x, grid_minor_line_y = grid_minor_line_y,
              legend_position = legend_position)
  class(out) <- "mschart_theme"
  out
}

#' @rdname set_theme
#' @export
#' @description Use \code{chart_theme()} to modify individual components of a chart theme.
chart_theme <- function( x, axis_title_x, axis_title_y, main_title,
                          axis_text_x, axis_text_y,
                          axis_ticks_x, axis_ticks_y,
                          grid_major_line_x, grid_major_line_y,
                          grid_minor_line_x, grid_minor_line_y,
                          date_fmt, str_fmt, double_fmt, integer_fmt, legend_position){

  if(!missing(axis_title_x)){
    if( !all( class( axis_title_x ) %in% class( x$theme$axis_title_x ) ) )
      stop("axis_title_x should be of class ", class( x$theme$axis_title_x ))
    x$theme$axis_title_x <- axis_title_x
  }

  if(!missing(axis_title_y)){
    if( !all( class( axis_title_y ) %in% class( x$theme$axis_title_y ) ) )
      stop("axis_title_y should be of class ", class( x$theme$axis_title_y ))
    x$theme$axis_title_y <- axis_title_y
  }

  if(!missing(main_title)){
    if( !all( class( main_title ) %in% class( x$theme$main_title ) ) )
      stop("main_title should be of class ", class( x$theme$main_title ))
    x$theme$main_title <- main_title
  }

  if(!missing(axis_text_x)){
    if( !all( class( axis_text_x ) %in% class( x$theme$axis_text_x ) ) )
      stop("axis_text_x should be of class ", class( x$theme$axis_text_x ))
    x$theme$axis_text_x <- axis_text_x
  }

  if(!missing(axis_text_y)){
    if( !all( class( axis_text_y ) %in% class( x$theme$axis_text_y ) ) )
      stop("axis_text_y should be of class ", class( x$theme$axis_text_y ))
    x$theme$axis_text_y <- axis_text_y
  }

  if(!missing(axis_ticks_x)){
    if( !all( class( axis_ticks_x ) %in% class( x$theme$axis_ticks_x ) ) )
      stop("axis_ticks_x should be of class ", class( x$theme$axis_ticks_x ))
    x$theme$axis_ticks_x <- axis_ticks_x
  }

  if(!missing(axis_ticks_y)){
    if( !all( class( axis_ticks_y ) %in% class( x$theme$axis_ticks_y ) ) )
      stop("axis_ticks_y should be of class ", class( x$theme$axis_ticks_y ))
    x$theme$axis_ticks_y <- axis_ticks_y
  }

  if(!missing(grid_major_line_x)){
    if( !all( class( grid_major_line_x ) %in% class( x$theme$grid_major_line_x ) ) )
      stop("grid_major_line_x should be of class ", class( x$theme$grid_major_line_x ))
    x$theme$grid_major_line_x <- grid_major_line_x
  }

  if(!missing(grid_major_line_y)){
    if( !all( class( grid_major_line_y ) %in% class( x$theme$grid_major_line_y ) ) )
      stop("grid_major_line_y should be of class ", class( x$theme$grid_major_line_y ))
    x$theme$grid_major_line_y <- grid_major_line_y
  }

  if(!missing(grid_minor_line_x)){
    if( !all( class( grid_minor_line_x ) %in% class( x$theme$grid_minor_line_x ) ) )
      stop("grid_minor_line_x should be of class ", class( x$theme$grid_minor_line_x ))
    x$theme$grid_minor_line_x <- grid_minor_line_x
  }

  if(!missing(grid_minor_line_y)){
    if( !all( class( grid_minor_line_y ) %in% class( x$theme$grid_minor_line_y ) ) )
      stop("grid_minor_line_y should be of class ", class( x$theme$grid_minor_line_y ))
    x$theme$grid_minor_line_y <- grid_minor_line_y
  }

  if(!missing(date_fmt)){
    if( !all( class( date_fmt ) %in% class( x$theme$date_fmt ) ) )
      stop("date_fmt should be of class ", class( x$theme$date_fmt ))
    x$theme$date_fmt <- date_fmt
  }

  if(!missing(str_fmt)){
    if( !all( class( str_fmt ) %in% class( x$theme$str_fmt ) ) )
      stop("str_fmt should be of class ", class( x$theme$str_fmt ))
    x$theme$str_fmt <- str_fmt
  }

  if(!missing(double_fmt)){
    if( !all( class( double_fmt ) %in% class( x$theme$double_fmt ) ) )
      stop("double_fmt should be of class ", class( x$theme$double_fmt ))
    x$theme$double_fmt <- double_fmt
  }

  if(!missing(integer_fmt)){
    if( !all( class( integer_fmt ) %in% class( x$theme$integer_fmt ) ) )
      stop("integer_fmt should be of class ", class( x$theme$integer_fmt ))
    x$theme$integer_fmt <- integer_fmt
  }

  if(!missing(legend_position)){
    if( !legend_position %in% st_legendpos ){
      stop("legend_position should be one of ", paste0(shQuote(st_legendpos), collapse = ", " ))
    }
  }

  x
}


