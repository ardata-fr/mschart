#' @title X axis settings
#' @description Define settings for an x axis.
#' @param x an `ms_chart` object.
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
#' @param num_fmt number formatting. See the num_fmt section for more details.
#' @param rotation rotation angle. Value should be between `-360`
#' and `360`.
#' @param limit_min minimum value on the axis. Date objects are also accepted
#' and will be converted automatically.
#' @param limit_max maximum value on the axis. Date objects are also accepted
#' and will be converted automatically.
#' @param position the value at which this axis crosses the perpendicular axis.
#' @param major_unit numeric, interval between major ticks and gridlines.
#' @param minor_unit numeric, interval between minor ticks and gridlines.
#' @param major_time_unit time unit for major ticks on date axes,
#' one of `"days"`, `"months"`, `"years"`.
#' @param minor_time_unit time unit for minor ticks on date axes,
#' one of `"days"`, `"months"`, `"years"`.
#' @section num_fmt:
#' All `%` need to be doubled, `0%%` means "a number
#' and percent symbol".
#'
#' To my current knowledge, depending on the chart type
#' and options, the following values are not systematically
#' used by office chart engine; i.e. when chart pre-compute
#' percentages, it seems using `0%%` will have no
#' effect.
#'
#' * `General`: default value
#' * `0`: display the number with no decimal
#' * `0.00`: display the number with two decimals
#' * `0%%`: display as percentages
#' * `0.00%%`: display as percentages with two decimal places
#' * `#,##0`
#' * `#,##0.00`
#' * `0.00E+00`
#' * `# ?/?`
#' * `# ??/??`
#' * `mm-dd-yy`
#' * `d-mmm-yy`
#' * `d-mmm`
#' * `mmm-yy`
#' * `h:mm AM/PM`
#' * `h:mm:ss AM/PM`
#' * `h:mm`
#' * `h:mm:ss`
#' * `m/d/yy h:mm`
#' * `#,##0 ;(#,##0)`
#' * `#,##0 ;[Red](#,##0)`
#' * `#,##0.00;(#,##0.00)`
#' * `#,##0.00;[Red](#,##0.00)`
#' * `mm:ss`
#' * `[h]:mm:ss`
#' * `mmss.0`
#' * `##0.0E+0`
#' * `@`
#'
#' @return An `ms_chart` object.
#' @export
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_chart_ax_x_1.png}{options: width="500"}}
#' @examples
#' library(mschart)
#'
#' chart_01 <- ms_linechart(
#'   data = us_indus_prod,
#'   x = "date", y = "value",
#'   group = "type"
#' )
#'
#' chart_01 <- chart_ax_y(x = chart_01, limit_min = 20, limit_max = 120)
#' chart_01
#'
#' # control axis intervals
#' chart_01 <- chart_ax_x(chart_01,
#'   major_unit = 10, major_time_unit = "years"
#' )
#' chart_01 <- chart_ax_y(chart_01, major_unit = 20)
#' chart_01
#' @seealso [chart_ax_y()], [ms_areachart()], [ms_barchart()], [ms_scatterchart()],
#' [ms_linechart()]
chart_ax_x <- function(
  x,
  orientation,
  crosses,
  cross_between,
  major_tick_mark,
  minor_tick_mark,
  tick_label_pos,
  display,
  num_fmt,
  rotation,
  limit_min,
  limit_max,
  position,
  major_unit,
  minor_unit,
  major_time_unit,
  minor_time_unit
) {
  stopifnot(inherits(x, "ms_chart"))

  options <- list(
    orientation = ifelse(
      missing(orientation),
      x$x_axis$orientation,
      orientation
    ),
    axis_position = x$x_axis$axis_position,
    crosses = ifelse(missing(crosses), x$x_axis$crosses, crosses),
    cross_between = ifelse(
      missing(cross_between),
      x$x_axis$cross_between,
      cross_between
    ),
    major_tick_mark = ifelse(
      missing(major_tick_mark),
      x$x_axis$major_tick_mark,
      major_tick_mark
    ),
    minor_tick_mark = ifelse(
      missing(minor_tick_mark),
      x$x_axis$minor_tick_mark,
      minor_tick_mark
    ),
    tick_label_pos = ifelse(
      missing(tick_label_pos),
      x$x_axis$tick_label_pos,
      tick_label_pos
    ),
    delete = ifelse(missing(display), x$x_axis$delete, !display),
    rotation = ifelse(missing(rotation), x$x_axis$rotation, rotation)
  )
  if (missing(num_fmt)) {
    options$num_fmt <- x$x_axis$num_fmt
  } else {
    options$num_fmt <- num_fmt
  }

  if (missing(limit_min) && !is.null(x$x_axis$limit_min)) {
    options$limit_min <- x$x_axis$limit_min
  } else if (!missing(limit_min)) {
    if (inherits(limit_min, "Date")) {
      limit_min <- as.integer(limit_min - as.Date("1899-12-30"))
    }
    options$limit_min <- limit_min
  }
  if (missing(limit_max) && !is.null(x$x_axis$limit_max)) {
    options$limit_max <- x$x_axis$limit_max
  } else if (!missing(limit_max)) {
    if (inherits(limit_max, "Date")) {
      limit_max <- as.integer(limit_max - as.Date("1899-12-30"))
    }
    options$limit_max <- limit_max
  }
  if (missing(position) && !is.null(x$x_axis$position)) {
    options$position <- x$x_axis$position
  } else if (!missing(position)) {
    options$position <- position
  }
  if (missing(major_unit) && !is.null(x$x_axis$major_unit)) {
    options$major_unit <- x$x_axis$major_unit
  } else if (!missing(major_unit)) {
    options$major_unit <- major_unit
  }
  if (missing(minor_unit) && !is.null(x$x_axis$minor_unit)) {
    options$minor_unit <- x$x_axis$minor_unit
  } else if (!missing(minor_unit)) {
    options$minor_unit <- minor_unit
  }
  if (missing(major_time_unit) && !is.null(x$x_axis$major_time_unit)) {
    options$major_time_unit <- x$x_axis$major_time_unit
  } else if (!missing(major_time_unit)) {
    options$major_time_unit <- major_time_unit
  }
  if (missing(minor_time_unit) && !is.null(x$x_axis$minor_time_unit)) {
    options$minor_time_unit <- x$x_axis$minor_time_unit
  } else if (!missing(minor_time_unit)) {
    options$minor_time_unit <- minor_time_unit
  }

  x$x_axis <- do.call(axis_options, options)
  x
}


#' @title Y axis settings
#' @description Define settings for a y axis.
#' @inheritParams chart_ax_x
#' @inheritSection chart_ax_x num_fmt
#' @return An `ms_chart` object.
#' @export
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_chart_ax_y_1.png}{options: width="500"}}
#' @examples
#' library(officer)
#' library(mschart)
#'
#' chart_01 <- ms_linechart(
#'   data = us_indus_prod,
#'   x = "date", y = "value",
#'   group = "type"
#' )
#' chart_01 <- chart_settings(chart_01, style = "marker")
#' chart_01 <- chart_ax_x(
#'   x = chart_01, num_fmt = "[$-fr-FR]mmm yyyy",
#'   limit_min = min(us_indus_prod$date),
#'   limit_max = as.Date("1992-01-01")
#' )
#' chart_01
#' @seealso [chart_ax_x()], [ms_areachart()], [ms_barchart()], [ms_scatterchart()],
#' [ms_linechart()]
chart_ax_y <- function(
  x,
  orientation,
  crosses,
  cross_between,
  major_tick_mark,
  minor_tick_mark,
  tick_label_pos,
  display,
  num_fmt,
  rotation,
  limit_min,
  limit_max,
  position,
  major_unit,
  minor_unit,
  major_time_unit,
  minor_time_unit
) {
  stopifnot(inherits(x, "ms_chart"))

  options <- list(
    orientation = ifelse(
      missing(orientation),
      x$y_axis$orientation,
      orientation
    ),
    axis_position = x$y_axis$axis_position,
    crosses = ifelse(missing(crosses), x$y_axis$crosses, crosses),
    cross_between = ifelse(
      missing(cross_between),
      x$y_axis$cross_between,
      cross_between
    ),
    major_tick_mark = ifelse(
      missing(major_tick_mark),
      x$y_axis$major_tick_mark,
      major_tick_mark
    ),
    minor_tick_mark = ifelse(
      missing(minor_tick_mark),
      x$y_axis$minor_tick_mark,
      minor_tick_mark
    ),
    tick_label_pos = ifelse(
      missing(tick_label_pos),
      x$y_axis$tick_label_pos,
      tick_label_pos
    ),
    delete = ifelse(missing(display), x$y_axis$delete, !display),
    rotation = ifelse(missing(rotation), x$y_axis$rotation, rotation)
  )
  if (missing(num_fmt)) {
    options$num_fmt <- x$y_axis$num_fmt
  } else {
    options$num_fmt <- num_fmt
  }
  if (missing(limit_min) && !is.null(x$y_axis$limit_min)) {
    options$limit_min <- x$y_axis$limit_min
  } else if (!missing(limit_min)) {
    if (inherits(limit_min, "Date")) {
      limit_min <- as.integer(limit_min - as.Date("1899-12-30"))
    }
    options$limit_min <- limit_min
  }
  if (missing(limit_max) && !is.null(x$y_axis$limit_max)) {
    options$limit_max <- x$y_axis$limit_max
  } else if (!missing(limit_max)) {
    if (inherits(limit_max, "Date")) {
      limit_max <- as.integer(limit_max - as.Date("1899-12-30"))
    }
    options$limit_max <- limit_max
  }
  if (missing(position) && !is.null(x$y_axis$position)) {
    options$position <- x$y_axis$position
  } else if (!missing(position)) {
    options$position <- position
  }
  if (missing(major_unit) && !is.null(x$y_axis$major_unit)) {
    options$major_unit <- x$y_axis$major_unit
  } else if (!missing(major_unit)) {
    options$major_unit <- major_unit
  }
  if (missing(minor_unit) && !is.null(x$y_axis$minor_unit)) {
    options$minor_unit <- x$y_axis$minor_unit
  } else if (!missing(minor_unit)) {
    options$minor_unit <- minor_unit
  }
  if (missing(major_time_unit) && !is.null(x$y_axis$major_time_unit)) {
    options$major_time_unit <- x$y_axis$major_time_unit
  } else if (!missing(major_time_unit)) {
    options$major_time_unit <- major_time_unit
  }
  if (missing(minor_time_unit) && !is.null(x$y_axis$minor_time_unit)) {
    options$minor_time_unit <- x$y_axis$minor_time_unit
  } else if (!missing(minor_time_unit)) {
    options$minor_time_unit <- minor_time_unit
  }

  x$y_axis <- do.call(axis_options, options)
  x
}


axis_options <- function(
  orientation = "minMax",
  axis_position = "b",
  crosses = "autoZero",
  cross_between = "between",
  major_tick_mark = "cross",
  minor_tick_mark = "none",
  tick_label_pos = "nextTo",
  delete = FALSE,
  num_fmt = NULL,
  rotation = 0,
  limit_min = NULL,
  limit_max = NULL,
  position = NULL,
  major_unit = NULL,
  minor_unit = NULL,
  major_time_unit = NULL,
  minor_time_unit = NULL
) {
  if (!orientation %in% st_orientation) {
    stop(
      "orientation should be one of ",
      paste0(shQuote(st_orientation), collapse = ", ")
    )
  }
  if (!axis_position %in% st_axpos) {
    stop(
      "axis_position should be one of ",
      paste0(shQuote(st_axpos), collapse = ", ")
    )
  }
  if (!crosses %in% st_crosses) {
    stop(
      "crosses should be one of ",
      paste0(shQuote(st_crosses), collapse = ", ")
    )
  }
  if (!cross_between %in% st_crossbetween) {
    stop(
      "cross_between should be one of ",
      paste0(shQuote(st_crossbetween), collapse = ", ")
    )
  }
  if (!major_tick_mark %in% st_tickmark) {
    stop(
      "major_tick_mark should be one of ",
      paste0(shQuote(st_tickmark), collapse = ", ")
    )
  }
  if (!minor_tick_mark %in% st_tickmark) {
    stop(
      "minor_tick_mark should be one of ",
      paste0(shQuote(st_tickmark), collapse = ", ")
    )
  }
  if (!tick_label_pos %in% st_ticklblpos) {
    stop(
      "tick_label_pos should be one of ",
      paste0(shQuote(st_ticklblpos), collapse = ", ")
    )
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
    position = position,
    major_unit = major_unit,
    minor_unit = minor_unit,
    major_time_unit = major_time_unit,
    minor_time_unit = minor_time_unit
  )
  class(out) <- "axis_options"
  out
}
