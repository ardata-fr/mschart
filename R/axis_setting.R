#' @title axis settings
#' @description Set axis properties.
#' @param x chart object
#' @param orientation axis orientation, expected value is one
#' of "maxMin" or "minMax".
#' @param crosses specifies how the axis crosses the perpendicular axis,
#' expected value should be one of "autoZero", "max" or "min".
#' @param cross_between specifies how the value axis crosses the category
#' axis between categories, expected value should be one of "midCat" or "between".
#' @param major_tick_mark,minor_tick_mark tick marks position, expected value should
#' be one of 'cross', 'in', 'none' or 'out'.
#' @param tick_label_pos ticks labels position, expected value should
#' be one of 'high', 'low', 'nextTo' or 'none'.
#' @param display should the axis be displayed.
#' @param num_fmt number formatting. It can be "General", "0.00", "#,##0",
#' "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param rotation rotation angle.
#' @param second_axis unused
#' @rdname set_axis
#' @export
set_x_axis <- function( x, orientation, crosses, cross_between,
                            major_tick_mark, minor_tick_mark,
                            tick_label_pos, display,
                            num_fmt, rotation, second_axis = FALSE ){
  UseMethod("set_x_axis")
}


#' @export
#' @rdname set_axis
set_y_axis <- function( x, orientation, crosses, cross_between,
                            major_tick_mark, minor_tick_mark,
                            tick_label_pos, display,
                            num_fmt, rotation, second_axis = FALSE ){
  UseMethod("set_y_axis")
}


#' @export
#' @rdname set_axis
set_x_axis.ms_chart <- function( x, orientation, crosses, cross_between,
                                     major_tick_mark, minor_tick_mark,
                                     tick_label_pos, display,
                                     num_fmt, rotation, second_axis = FALSE ){
  options <- axis_options( orientation = ifelse(missing(orientation), x$x_axis$orientation, orientation),
                           axis_position = ifelse( second_axis, "r", "l" ),
                           crosses = ifelse(missing(crosses), x$x_axis$crosses, crosses),
                           cross_between = ifelse(missing(cross_between), x$x_axis$cross_between, cross_between),
                           major_tick_mark = ifelse(missing(major_tick_mark), x$x_axis$major_tick_mark, major_tick_mark),
                           minor_tick_mark = ifelse(missing(minor_tick_mark), x$x_axis$minor_tick_mark, minor_tick_mark),
                           tick_label_pos = ifelse(missing(tick_label_pos), x$x_axis$tick_label_pos, tick_label_pos),
                           delete = ifelse(missing(display), x$x_axis$delete, !display),
                           num_fmt = ifelse(missing(num_fmt), x$x_axis$num_fmt, num_fmt),
                           rotation = ifelse(missing(rotation), x$x_axis$rotation, rotation) )

  x$x_axis <- options
  x
}

#' @export
set_y_axis.ms_chart <- function( x, orientation, crosses, cross_between,
                                 major_tick_mark, minor_tick_mark,
                                 tick_label_pos, display,
                                 num_fmt, rotation, second_axis = FALSE ){

  options <- axis_options( orientation = ifelse(missing(orientation), x$y_axis$orientation, orientation),
                           axis_position = ifelse( second_axis, "t", "b" ),
                           crosses = ifelse(missing(crosses), x$y_axis$crosses, crosses),
                           cross_between = ifelse(missing(cross_between), x$y_axis$cross_between, cross_between),
                           major_tick_mark = ifelse(missing(major_tick_mark), x$y_axis$major_tick_mark, major_tick_mark),
                           minor_tick_mark = ifelse(missing(minor_tick_mark), x$y_axis$minor_tick_mark, minor_tick_mark),
                           tick_label_pos = ifelse(missing(tick_label_pos), x$y_axis$tick_label_pos, tick_label_pos),
                           delete = ifelse(missing(display), x$y_axis$delete, !display),
                           num_fmt = ifelse(missing(num_fmt), x$y_axis$num_fmt, num_fmt),
                           rotation = ifelse(missing(rotation), x$y_axis$rotation, rotation) )

  x$y_axis <- options
  x
}




axis_options <- function( orientation = "minMax", axis_position = "b",
                          crosses = "autoZero", cross_between = "between",
                          major_tick_mark = "cross", minor_tick_mark = "none",
                          tick_label_pos = "nextTo", delete = FALSE, num_fmt = "General",
                          rotation = 0 ){

  if( !orientation %in% st_orientation ){
    stop("orientation should be one of ", paste0(shQuote(st_orientation), collapse = ", " ))
  }
  if( !axis_position %in% st_axpos ){
    stop("axis_position should be one of ", paste0(shQuote(st_axpos), collapse = ", " ))
  }
  if( !crosses %in% st_crosses ){
    stop("crosses should be one of ", paste0(shQuote(st_crosses), collapse = ", " ))
  }
  if( !cross_between %in% st_crossbetween ){
    stop("cross_between should be one of ", paste0(shQuote(st_crossbetween), collapse = ", " ))
  }
  if( !major_tick_mark %in% st_tickmark ){
    stop("major_tick_mark should be one of ", paste0(shQuote(st_tickmark), collapse = ", " ))
  }
  if( !minor_tick_mark %in% st_tickmark ){
    stop("minor_tick_mark should be one of ", paste0(shQuote(st_tickmark), collapse = ", " ))
  }
  if( !tick_label_pos %in% st_ticklblpos ){
    stop("tick_label_pos should be one of ", paste0(shQuote(st_ticklblpos), collapse = ", " ))
  }

  out <- list(
    orientation = orientation,
    axis_position = axis_position,
    crosses = crosses,
    cross_between = cross_between,
    delete = delete,
    num_fmt = num_fmt,
    major_tick_mark = major_tick_mark,
    minor_tick_mark = minor_tick_mark,
    tick_label_pos = tick_label_pos,
    rotation = rotation
  )
  class(out) <- "axis_options"
  out

}


