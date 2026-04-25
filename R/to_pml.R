clustered_pos <- c("ctr", "inBase", "inEnd", "outEnd")
stacked_pos <- c("ctr", "inBase", "inEnd")

#' @export
#' @method to_pml ms_barchart
to_pml.ms_barchart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  if ("clustered" %in% x$options$grouping) {
    if (!x$label_settings$position %in% clustered_pos) {
      stop(
        "label position issue with grouping 'clustered'.",
        "Arg. position in chart_data_labels() should match one of ",
        paste(shQuote(clustered_pos), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if ("stacked" %in% x$options$grouping) {
    if (!x$label_settings$position %in% stacked_pos) {
      stop(
        "label position issue with grouping 'clustered'.",
        "Arg. position in chart_data_labels() should match one of ",
        paste(shQuote(stacked_pos), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[x$y]]),
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  str_series_ <- sapply(series, function(serie, template) {
    marker_str <- get_sppr_xml(serie$fill, serie$stroke, serie$line_width)

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if (!is.null(x$label_cols)) {
      label_pml <- to_pml(serie$label)
    } else {
      label_pml <- ""
    }

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", max(0, serie$idx)),
      sprintf("<c:order val=\"%.0f\"/>", max(0, serie$order)),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      marker_str,
      "<c:invertIfNegative val=\"0\"/>",
      to_pml(label_settings, show_label = !is.null(x$label_cols)),
      "<c:cat>",
      to_pml(serie$x),
      "</c:cat>",
      "<c:val>",
      to_pml(serie$y),
      "</c:val>",
      label_pml,
      "</c:ser>"
    )
  })
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  dir_ <- structure(c("bar", "col"), .Names = c("horizontal", "vertical"))
  dir_ <- dir_[x$options$dir]

  paste0(
    "<c:barChart>",
    sprintf("<c:barDir val=\"%s\"/>", dir_),
    sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(x$label_settings, !is.null(x$label_cols)),
    sprintf("<c:gapWidth val=\"%.0f\"/>", x$options$gap_width),
    sprintf("<c:overlap val=\"%.0f\"/>", x$options$overlap),
    x_ax_id,
    y_ax_id,
    "</c:barChart>"
  )
}

standard_pos <- c("b", "ctr", "l", "r", "t")

#' @export
#' @method to_pml ms_linechart
to_pml.ms_linechart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  if (!x$label_settings$position %in% standard_pos) {
    stop(
      "label position issue.",
      "Arg. position in chart_data_labels() should match one of ",
      paste(shQuote(standard_pos), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[x$y]]),
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  # sapply linec-----
  str_series_ <- sapply(
    series,
    function(serie, has_line, has_marker) {
      if (!has_line) {
        line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
      } else {
        line_properties <- fp_border(
          color = serie$stroke,
          style = serie$line_style,
          width = serie$line_width
        )
        line_str <- ooxml_fp_border(line_properties, in_tags = c("c:spPr"))
      }
      if (!has_marker) {
        marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
      } else {
        marker_str <- get_marker_xml(
          serie$fill,
          serie$stroke,
          serie$symbol,
          serie$size
        )
      }

      label_settings <- x$label_settings
      label_settings$labels_fp <- serie$labels_fp

      if (!is.null(x$label_cols)) {
        label_pml <- to_pml(serie$label)
      } else {
        label_pml <- ""
      }

      paste0(
        "<c:ser>",
        sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
        sprintf("<c:order val=\"%.0f\"/>", serie$order),
        sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
        line_str,
        marker_str,
        to_pml(label_settings, show_label = !is.null(x$label_cols)),
        "<c:cat>",
        to_pml(serie$x),
        "</c:cat>",
        "<c:val>",
        to_pml(serie$y),
        "</c:val>",
        label_pml,
        sprintf("<c:smooth val=\"%.0f\"/>", serie$smooth),
        "</c:ser>"
      )
    },
    has_line = has_lines[x$options$style],
    has_marker = has_markers[x$options$style]
  )

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:lineChart>",
    sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(x$label_settings),
    x_ax_id,
    y_ax_id,
    "</c:lineChart>"
  )
}


#' @export
#' @method to_pml ms_stockchart
to_pml.ms_stockchart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[".mschart_y"]]),
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  str_series_ <- sapply(series, function(serie) {
    line_properties <- fp_border(
      color = serie$stroke,
      style = serie$line_style,
      width = serie$line_width
    )
    line_str <- ooxml_fp_border(line_properties, in_tags = c("c:spPr"))
    marker_str <- get_marker_xml(
      serie$fill, serie$stroke,
      serie$symbol, serie$size
    )

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      line_str,
      marker_str,
      "<c:cat>", to_pml(serie$x), "</c:cat>",
      "<c:val>", to_pml(serie$y), "</c:val>",
      "<c:smooth val=\"0\"/>",
      "</c:ser>"
    )
  })

  str_series_ <- paste(str_series_, collapse = "")

  # hiLowLines
  hi_low_str <- ""
  if (!isFALSE(x$options$hi_low_lines)) {
    hi_low_str <- ooxml_fp_border(
      x$options$hi_low_lines,
      in_tags = c("c:hiLowLines", "c:spPr")
    )
  }

  # upDownBars (OHLC only)
  up_down_str <- ""
  has_open <- !is.null(x$stock_cols$open)
  if (has_open) {
    up_fill <- stock_fill_xml(x$options$up_bars_fill)
    up_border <- ooxml_fp_border(x$options$up_bars_border)
    down_fill <- stock_fill_xml(x$options$down_bars_fill)
    down_border <- ooxml_fp_border(x$options$down_bars_border)

    up_down_str <- paste0(
      "<c:upDownBars>",
      "<c:gapWidth val=\"150\"/>",
      "<c:upBars><c:spPr>", up_fill, up_border, "</c:spPr></c:upBars>",
      "<c:downBars><c:spPr>", down_fill, down_border, "</c:spPr></c:downBars>",
      "</c:upDownBars>"
    )
  }

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:stockChart>",
    str_series_,
    "<c:dLbls>",
    "<c:showLegendKey val=\"0\"/>",
    "<c:showVal val=\"0\"/>",
    "<c:showCatName val=\"0\"/>",
    "<c:showSerName val=\"0\"/>",
    "<c:showPercent val=\"0\"/>",
    "<c:showBubbleSize val=\"0\"/>",
    "</c:dLbls>",
    hi_low_str,
    up_down_str,
    x_ax_id,
    y_ax_id,
    "</c:stockChart>"
  )
}

#' @export
#' @method to_pml ms_radarchart
to_pml.ms_radarchart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[x$y]]),
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  has_lines <- c(
    standard = TRUE, marker = TRUE, filled = TRUE
  )
  has_markers <- c(
    standard = FALSE, marker = TRUE, filled = FALSE
  )

  str_series_ <- sapply(
    series,
    function(serie, has_line, has_marker) {
      if (!has_line) {
        line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
      } else {
        line_properties <- fp_border(
          color = serie$stroke,
          style = serie$line_style,
          width = serie$line_width
        )
        line_str <- ooxml_fp_border(line_properties, in_tags = c("c:spPr"))
      }
      if (!has_marker) {
        marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
      } else {
        marker_str <- get_marker_xml(
          serie$fill, serie$stroke,
          serie$symbol, serie$size
        )
      }

      label_settings <- x$label_settings
      label_settings$labels_fp <- serie$labels_fp

      label_pml <- ""
      if (!is.null(x$label_cols)) {
        label_pml <- to_pml(serie$label)
      }

      paste0(
        "<c:ser>",
        sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
        sprintf("<c:order val=\"%.0f\"/>", serie$order),
        sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
        line_str,
        marker_str,
        to_pml(label_settings,
               with_position = FALSE,
               show_label = !is.null(x$label_cols)),
        "<c:cat>", to_pml(serie$x), "</c:cat>",
        "<c:val>", to_pml(serie$y), "</c:val>",
        "</c:ser>"
      )
    },
    has_line = has_lines[x$options$radarstyle],
    has_marker = has_markers[x$options$radarstyle]
  )

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:radarChart>",
    sprintf("<c:radarStyle val=\"%s\"/>", x$options$radarstyle),
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(x$label_settings,
           with_position = FALSE,
           show_label = !is.null(x$label_cols)),
    x_ax_id,
    y_ax_id,
    "</c:radarChart>"
  )
}

#' @export
#' @method to_pml ms_bubblechart
to_pml.ms_bubblechart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  x_class <- serie_builtin_class(x$data[[x$x]])
  y_class <- serie_builtin_class(x$data[[x$y]])

  series <- as_series(
    x,
    x_class = x_class,
    y_class = y_class,
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  str_series_ <- sapply(series, function(serie) {
    sppr_str <- get_sppr_xml(serie$fill, serie$stroke, serie$line_width)

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    label_pml <- ""
    if (!is.null(x$label_cols) && !is.null(serie$label)) {
      label_pml <- to_pml(serie$label)
    }

    bubble_size_str <- ""
    if (!is.null(serie$bubble_size)) {
      bubble_size_str <- paste0(
        "<c:bubbleSize>", to_pml(serie$bubble_size), "</c:bubbleSize>"
      )
    }

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      sppr_str,
      to_pml(label_settings, show_label = !is.null(x$label_cols)),
      "<c:invertIfNegative val=\"0\"/>",
      "<c:xVal>", to_pml(serie$x), "</c:xVal>",
      "<c:yVal>", to_pml(serie$y), "</c:yVal>",
      bubble_size_str,
      sprintf(
        "<c:bubble3D val=\"%.0f\"/>",
        x$options$bubble3D %||% 0
      ),
      label_pml,
      "</c:ser>"
    )
  })

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:bubbleChart>",
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(x$label_settings, !is.null(x$label_cols)),
    x_ax_id,
    y_ax_id,
    "</c:bubbleChart>"
  )
}

#' @export
#' @method to_pml ms_areachart
to_pml.ms_areachart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[x$y]]),
    sheetname = sheetname,
    secondary_y = secondary_y
  )

  str_series_ <- sapply(series, function(serie) {
    marker_str <- get_sppr_xml(serie$fill, serie$stroke, serie$line_width)

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if (!is.null(x$label_cols)) {
      label_pml <- to_pml(serie$label)
    } else {
      label_pml <- ""
    }

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      "<c:tx>",
      to_pml(serie$tx),
      "</c:tx>",
      marker_str,
      to_pml(
        label_settings,
        with_position = FALSE,
        show_label = !is.null(x$label_cols)
      ),
      "<c:cat>",
      to_pml(serie$x),
      "</c:cat>",
      "<c:val>",
      to_pml(serie$y),
      "</c:val>",
      label_pml,
      "</c:ser>"
    )
  })
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:areaChart>",
    sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(
      x$label_settings,
      with_position = FALSE,
      show_label = !is.null(x$label_cols)
    ),
    x_ax_id,
    y_ax_id,
    "</c:areaChart>"
  )
}

scatterstyles <- c(
  'none',
  'line',
  'lineMarker',
  'marker',
  'smooth',
  'smoothMarker'
)
has_markers <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
names(has_markers) <- scatterstyles
has_lines <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
names(has_lines) <- scatterstyles

#' @export
#' @method to_pml ms_scatterchart
to_pml.ms_scatterchart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  if (!x$label_settings$position %in% standard_pos) {
    stop(
      "label position issue.",
      "Arg. position in chart_data_labels() should match one of ",
      paste(shQuote(standard_pos), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (isTRUE(x$asis)) {
    series <- as_series(
      x,
      x_class = serie_builtin_class(sort(unname(unlist(x$data_series[
        x$xvar
      ])))),
      y_class = serie_builtin_class(sort(unname(unlist(x$data_series[
        x$yvar
      ])))),
      sheetname = sheetname,
      secondary_y = secondary_y
    )
  } else {
    series <- as_series(
      x,
      x_class = serie_builtin_class(x$data[[x$x]]),
      y_class = serie_builtin_class(x$data[[x$y]]),
      sheetname = sheetname
    )
  }

  str_series_ <- sapply(
    series,
    function(serie, has_line, has_marker) {
      if (!has_line || serie$line_style %in% "none") {
        line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
      } else {
        line_properties <- fp_border(
          color = serie$stroke,
          style = serie$line_style,
          width = serie$line_width
        )
        line_str <- ooxml_fp_border(line_properties, in_tags = c("c:spPr"))
      }
      if (!has_marker) {
        marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
      } else {
        marker_str <- get_marker_xml(
          serie$fill,
          serie$stroke,
          serie$symbol,
          serie$size
        )
      }

      label_settings <- x$label_settings
      label_settings$labels_fp <- serie$labels_fp

      if (!is.null(x$label_cols)) {
        label_pml <- to_pml(serie$label)
      } else {
        label_pml <- ""
      }

      paste0(
        "<c:ser>",
        sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
        sprintf("<c:order val=\"%.0f\"/>", serie$order),
        sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
        line_str,
        marker_str,
        to_pml(label_settings, show_label = !is.null(x$label_cols)),
        "<c:xVal>",
        to_pml(serie$x),
        "</c:xVal>",
        "<c:yVal>",
        to_pml(serie$y),
        "</c:yVal>",
        label_pml,
        sprintf("<c:smooth val=\"%.0f\"/>", serie$smooth),
        "</c:ser>"
      )
    },
    has_line = has_lines[x$options$style],
    has_marker = has_markers[x$options$style]
  )

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0(
    "<c:scatterChart>",
    sprintf("<c:scatterStyle val=\"%s\"/>", x$options$style),
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(x$label_settings, !is.null(x$label_cols)),
    x_ax_id,
    y_ax_id,
    "</c:scatterChart>"
  )
}


#' @export
#' @method to_pml ms_piechart
to_pml.ms_piechart <- function(
  x,
  add_ns = FALSE,
  id_x,
  id_y,
  sheetname = "sheet1",
  secondary_y = 0,
  ...
) {
  series <- as_series(
    x,
    x_class = serie_builtin_class(x$data[[x$x]]),
    y_class = serie_builtin_class(x$data[[x$y]]),
    sheetname = sheetname
  )

  str_series_ <- sapply(series, function(serie) {
    cats <- as.character(x$data[[x$x]])
    n <- length(cats)

    dpt_str <- vapply(
      seq_len(n),
      function(i) {
        cat_i <- cats[i]
        paste0(
          sprintf("<c:dPt><c:idx val=\"%s\"/><c:bubble3D val=\"0\"/>", i - 1),
          get_sppr_xml(
            x$series_settings$fill[[cat_i]],
            x$series_settings$colour[[cat_i]],
            x$series_settings$line_width[[cat_i]]
          ),
          "</c:dPt>"
        )
      },
      character(1)
    )
    dpt_str <- paste0(dpt_str, collapse = "")

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    label_pml <- if (!is.null(x$label_cols)) to_pml(serie$label) else ""

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      "<c:tx>",
      to_pml(serie$tx),
      "</c:tx>",
      dpt_str,
      to_pml(
        label_settings,
        with_position = FALSE,
        show_label = !is.null(x$label_cols)
      ),
      "<c:cat>",
      to_pml(serie$x),
      "</c:cat>",
      "<c:val>",
      to_pml(serie$y),
      "</c:val>",
      label_pml,
      "</c:ser>"
    )
  })
  str_series_ <- paste(str_series_, collapse = "")

  is_donut <- x$options$hole_size > 0
  tag <- if (is_donut) "c:doughnutChart" else "c:pieChart"
  hole_str <- if (is_donut) {
    sprintf("<c:holeSize val=\"%.0f\"/>", x$options$hole_size)
  } else {
    ""
  }

  paste0(
    "<",
    tag,
    ">",
    sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
    str_series_,
    to_pml(
      x$label_settings,
      with_position = FALSE,
      show_label = !is.null(x$label_cols)
    ),
    hole_str,
    "</",
    tag,
    ">"
  )
}


#' @importFrom grDevices col2rgb
get_marker_xml <- function(fill, stroke, symbol = NULL, size = NULL) {
  fill_elts <- col2rgb(fill, alpha = TRUE)[, 1]
  fill_hex <- sprintf("%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3])
  stroke_elts <- col2rgb(stroke, alpha = TRUE)[, 1]
  stroke_hex <- sprintf(
    "%02X%02X%02X",
    stroke_elts[1],
    stroke_elts[2],
    stroke_elts[3]
  )

  if (!is.null(size) && size < 1) {
    symbol <- "none"
    size <- 2L
  }

  str_sym <- ""
  if (!is.null(symbol)) {
    str_sym <- sprintf("<c:symbol val=\"%s\"/>", symbol)
  }

  str_size <- ""
  if (!is.null(size)) {
    str_size <- sprintf("<c:size val=\"%.0f\"/>", size)
  }

  paste0(
    "<c:marker>",
    str_sym,
    str_size,
    get_sppr_xml(fill, stroke),
    "</c:marker>"
  )
}


get_sppr_xml <- function(fill, stroke, line_width = NULL) {
  fill_elts <- col2rgb(fill, alpha = TRUE)[, 1]
  fill_hex <- sprintf("%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3])
  stroke_elts <- col2rgb(stroke, alpha = TRUE)[, 1]
  stroke_hex <- sprintf(
    "%02X%02X%02X",
    stroke_elts[1],
    stroke_elts[2],
    stroke_elts[3]
  )
  stroke_width <- ""
  if (!is.null(line_width)) {
    stroke_width <- sprintf(" w=\"%.0f\"", 12700 * line_width)
  }
  paste0(
    "<c:spPr>",
    sprintf(
      "<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>",
      fill_hex,
      fill_elts[4] / 255.0 * 100000
    ),
    sprintf(
      "<a:ln%s><a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill></a:ln>",
      stroke_width,
      stroke_hex,
      stroke_elts[4] / 255.0 * 100000
    ),
    "<a:effectLst/></c:spPr>"
  )
}
