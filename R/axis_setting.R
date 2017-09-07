#' @title x axis settings
#' @description Define settings for an x axis.
#' @param x an \code{ms_chart} object.
#' @param orientation axis orientation, one of 'maxMin', 'minMax'.
#' @param crosses specifies how the axis crosses the perpendicular
#' axis, one of 'autoZero', 'max', 'min'.
#' @param cross_between specifies how the value axis crosses the
#' category axis between categories, one of 'between', 'midCat'.
#' @param major_tick_mark,minor_tick_mark tick marks position,
#' one of 'cross', 'in', 'none', 'out'.
#' @param tick_label_pos ticks labels position, one of 'high',
#' 'low', 'nextTo', 'none'.
#' @param display should the axis be displayed (a logical of
#' length 1).
#' @param num_fmt number formatting. It can be "General", "0.00",
#' "#,##0", "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param rotation rotation angle. Value should be between `-360`
#' and `360`.
#' @param limit_min minimum value on the axis.
#' @param limit_max maximum value on the axis.
#' @param position position value that cross the other axis.
#' @param second_axis unused
#' @seealso \code{\link{chart_ax_y}}
#' @export
chart_ax_x <- function( x, orientation, crosses, cross_between,
                            major_tick_mark, minor_tick_mark,
                            tick_label_pos, display,
                            num_fmt, rotation,
                        limit_min, limit_max, position,
                        second_axis = FALSE ){
  UseMethod("chart_ax_x")
}


#' @export
#' @title y axis settings
#' @description Set y axis properties.
#' @inheritParams chart_ax_x
#' @seealso \code{\link{chart_ax_x}}
chart_ax_y <- function( x, orientation, crosses, cross_between,
                        major_tick_mark, minor_tick_mark,
                        tick_label_pos, display,
                        num_fmt, rotation,
                        limit_min, limit_max, position,
                        second_axis = FALSE ){
  UseMethod("chart_ax_y")
}


#' @export
#' @describeIn chart_ax_x chart_ax_x method for ms_chart objects
chart_ax_x.ms_chart <- function( x, orientation, crosses, cross_between,
                                 major_tick_mark, minor_tick_mark,
                                 tick_label_pos, display,
                                 num_fmt, rotation,
                                 limit_min, limit_max, position,
                                 second_axis = FALSE ){


  options <- list( orientation = ifelse(missing(orientation), x$x_axis$orientation, orientation),
                           axis_position = ifelse( second_axis, "r", "l" ),
                           crosses = ifelse(missing(crosses), x$x_axis$crosses, crosses),
                           cross_between = ifelse(missing(cross_between), x$x_axis$cross_between, cross_between),
                           major_tick_mark = ifelse(missing(major_tick_mark), x$x_axis$major_tick_mark, major_tick_mark),
                           minor_tick_mark = ifelse(missing(minor_tick_mark), x$x_axis$minor_tick_mark, minor_tick_mark),
                           tick_label_pos = ifelse(missing(tick_label_pos), x$x_axis$tick_label_pos, tick_label_pos),
                           delete = ifelse(missing(display), x$x_axis$delete, !display),
                           num_fmt = ifelse(missing(num_fmt), x$x_axis$num_fmt, num_fmt),
                           rotation = ifelse(missing(rotation), x$x_axis$rotation, rotation)
                           )
  if( missing(limit_min) && !is.null(x$x_axis$limit_min) ){
    options$limit_min <- x$x_axis$limit_min
  } else if( !missing(limit_min) ){
    options$limit_min <- limit_min
  }
  if( missing(limit_max) && !is.null(x$x_axis$limit_max) ){
    options$limit_max <- x$x_axis$limit_max
  } else if( !missing(limit_max) ){
    options$limit_max <- limit_max
  }
  if( missing(position) && !is.null(x$x_axis$position) ){
    options$position <- x$x_axis$position
  } else if( !missing(position) ){
    options$position <- position
  }

  x$x_axis <- do.call(axis_options, options)
  x
}

#' @export
#' @describeIn chart_ax_y chart_ax_y method for ms_chart objects
chart_ax_y.ms_chart <- function( x, orientation, crosses, cross_between,
                                 major_tick_mark, minor_tick_mark,
                                 tick_label_pos, display,
                                 num_fmt, rotation,
                                 limit_min, limit_max, position,
                                 second_axis = FALSE ){

  options <- list( orientation = ifelse(missing(orientation), x$y_axis$orientation, orientation),
                   axis_position = ifelse( second_axis, "r", "l" ),
                   crosses = ifelse(missing(crosses), x$y_axis$crosses, crosses),
                   cross_between = ifelse(missing(cross_between), x$y_axis$cross_between, cross_between),
                   major_tick_mark = ifelse(missing(major_tick_mark), x$y_axis$major_tick_mark, major_tick_mark),
                   minor_tick_mark = ifelse(missing(minor_tick_mark), x$y_axis$minor_tick_mark, minor_tick_mark),
                   tick_label_pos = ifelse(missing(tick_label_pos), x$y_axis$tick_label_pos, tick_label_pos),
                   delete = ifelse(missing(display), x$y_axis$delete, !display),
                   num_fmt = ifelse(missing(num_fmt), x$y_axis$num_fmt, num_fmt),
                   rotation = ifelse(missing(rotation), x$y_axis$rotation, rotation)
  )
  if( missing(limit_min) && !is.null(x$y_axis$limit_min) ){
    options$limit_min <- x$y_axis$limit_min
  } else if( !missing(limit_min) ){
    options$limit_min <- limit_min
  }
  if( missing(limit_max) && !is.null(x$y_axis$limit_max) ){
    options$limit_max <- x$y_axis$limit_max
  } else if( !missing(limit_max) ){
    options$limit_max <- limit_max
  }
  if( missing(position) && !is.null(x$y_axis$position) ){
    options$position <- x$y_axis$position
  } else if( !missing(position) ){
    options$position <- position
  }

  x$y_axis <- do.call(axis_options, options)
  x
}




axis_options <- function( orientation = "minMax", axis_position = "b",
                          crosses = "autoZero", cross_between = "between",
                          major_tick_mark = "cross", minor_tick_mark = "none",
                          tick_label_pos = "nextTo", delete = FALSE, num_fmt = "General",
                          rotation = 0, limit_min = NULL, limit_max = NULL, position = NULL ){

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
    rotation = rotation,
    limit_min = limit_min,
    limit_max = limit_max,
    position = position
  )
  class(out) <- "axis_options"
  out

}


