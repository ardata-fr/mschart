clustered_pos <- c("ctr", "inBase", "inEnd", "outEnd")
stacked_pos <- c("ctr", "inBase", "inEnd")

#' @method to_pml ms_barchart
to_pml.ms_barchart <- function(x, id_x, id_y, sheetname = "sheet1", add_ns = FALSE, ...){

  if( "clustered" %in% x$options$grouping )
    if( !x$label_settings$position %in% clustered_pos ){
      stop("label position issue with grouping 'clustered'.",
           "Arg. position in chart_data_labels() should match one of ",
           paste(shQuote(clustered_pos), collapse = ", "), ".", call. = FALSE)
    }
  if( "stacked" %in% x$options$grouping )
    if( !x$label_settings$position %in% stacked_pos ){
      stop("label position issue with grouping 'clustered'.",
           "Arg. position in chart_data_labels() should match one of ",
           paste(shQuote(stacked_pos), collapse = ", "), ".", call. = FALSE)
    }

  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]), sheetname = sheetname )

  str_series_ <- sapply( series, function(serie, template ){
    marker_str <- get_sppr_xml(serie$fill, serie$stroke, serie$line_width )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if(!is.null(x$label_cols)){
      label_pml <- to_pml(serie$label)
    } else label_pml <- ""

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      marker_str,
      "<c:invertIfNegative val=\"0\"/>",
      to_pml(label_settings, show_label = !is.null(x$label_cols)),
      "<c:cat>", to_pml(serie$x), "</c:cat>",
      "<c:val>", to_pml(serie$y), "</c:val>",
      label_pml,
      "</c:ser>"
    )
  })
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  dir_ <- structure(c("bar", "col"), .Names = c("horizontal", "vertical"))
  dir_ <- dir_[x$options$dir]

  paste0( "<c:barChart>",
                  sprintf("<c:barDir val=\"%s\"/>", dir_),
                  sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
                  sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
                  str_series_,
                  to_pml(x$label_settings, !is.null(x$label_cols)),
                  sprintf("<c:gapWidth val=\"%.0f\"/>", x$options$gap_width),
                  sprintf("<c:overlap val=\"%.0f\"/>", x$options$overlap),
                  x_ax_id, y_ax_id,
                  "</c:barChart>" )
}

standard_pos <- c("b", "ctr", "l", "r", "t")
#' @method to_pml ms_linechart
to_pml.ms_linechart <- function(x, id_x, id_y, sheetname = "sheet1", add_ns = FALSE, ...){

  if( !x$label_settings$position %in% standard_pos ){
    stop("label position issue.",
         "Arg. position in chart_data_labels() should match one of ",
         paste(shQuote(standard_pos), collapse = ", "), ".", call. = FALSE)
  }

  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]), sheetname = sheetname )

  # sapply linec-----
  str_series_ <- sapply( series, function(serie, has_line, has_marker ){
    if( !has_line ){
      line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
    } else {
      line_properties <- fp_border(color = serie$stroke, style = serie$line_style, width = serie$line_width)
      line_str <- ooxml_fp_border(line_properties, in_tags = c("c:spPr"))
    }
    if( !has_marker )
      marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
    else marker_str <- get_marker_xml(serie$fill, serie$stroke, serie$symbol, serie$size )


    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if(!is.null(x$label_cols)){
      label_pml <- to_pml(serie$label)
    } else label_pml <- ""

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      line_str, marker_str,
      to_pml(label_settings, show_label = !is.null(x$label_cols)),
      "<c:cat>", to_pml(serie$x), "</c:cat>",
      "<c:val>", to_pml(serie$y), "</c:val>",
      label_pml,
      sprintf("<c:smooth val=\"%.0f\"/>", serie$smooth),
      "</c:ser>"
    )
  },
  has_line = has_lines[x$options$linestyle],
  has_marker = has_markers[x$options$linestyle])

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:lineChart>",
                  sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
                  sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
                  str_series_,
                  to_pml(x$label_settings),
                  x_ax_id, y_ax_id,
                  "</c:lineChart>"  )
}

#' @method to_pml ms_areachart
to_pml.ms_areachart <- function(x, id_x, id_y, sheetname = "sheet1", add_ns = FALSE, ...){

  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]), sheetname = sheetname )

  str_series_ <- sapply( series, function(serie){
    marker_str <- get_sppr_xml(serie$fill, serie$stroke, serie$line_width)

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if(!is.null(x$label_cols)){
      label_pml <- to_pml(serie$label)
    } else label_pml <- ""

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      "<c:tx>", to_pml(serie$tx), "</c:tx>",
      marker_str,
      to_pml(label_settings, with_position = FALSE, show_label = !is.null(x$label_cols)),
      "<c:cat>", to_pml(serie$x), "</c:cat>",
      "<c:val>", to_pml(serie$y), "</c:val>",
      label_pml,
      "</c:ser>"
    )
  })
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:areaChart>",
          sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
          sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
          str_series_,
          to_pml(x$label_settings, with_position = FALSE, show_label = !is.null(x$label_cols)),
          x_ax_id, y_ax_id,
          "</c:areaChart>"  )
}

scatterstyles <- c('none', 'line', 'lineMarker', 'marker', 'smooth', 'smoothMarker')
has_markers <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
names(has_markers) <- scatterstyles
has_lines <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
names(has_lines) <- scatterstyles

#' @method to_pml ms_scatterchart
to_pml.ms_scatterchart <- function(x, id_x, id_y, sheetname = "sheet1", add_ns = FALSE, asis = FALSE, ...){

  if( !x$label_settings$position %in% standard_pos ){
    stop("label position issue.",
         "Arg. position in chart_data_labels() should match one of ",
         paste(shQuote(standard_pos), collapse = ", "), ".", call. = FALSE)
  }

  if (asis)
    series <- as_series(
      x,
      x_class = serie_builtin_class(sort(unname(unlist(x$data_series[x$xvar])))),
      y_class = serie_builtin_class(sort(unname(unlist(x$data_series[x$yvar])))),
      sheetname = sheetname
    )
  else
    series <- as_series(
      x,
      x_class = serie_builtin_class(x$data[[x$x]]),
      y_class = serie_builtin_class(x$data[[x$y]]),
      sheetname = sheetname
    )

  str_series_ <- sapply( series, function(serie, has_line, has_marker ){

    if( !has_line || serie$line_style %in% "none" || serie$line_width < 1){
      line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
    } else {
      line_properties <- fp_border(color = serie$stroke, style = serie$line_style, width = serie$line_width)
      line_str <- ooxml_fp_border(line_properties,
                      in_tags = c("c:spPr"))
    }
    if( !has_marker )
      marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
    else marker_str <- get_marker_xml(serie$fill, serie$stroke, serie$symbol, serie$size )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp

    if(!is.null(x$label_cols)){
      label_pml <- to_pml(serie$label)
    } else label_pml <- ""

    paste0(
      "<c:ser>",
      sprintf("<c:idx val=\"%.0f\"/>", serie$idx),
      sprintf("<c:order val=\"%.0f\"/>", serie$order),
      sprintf("<c:tx>%s</c:tx>", to_pml(serie$tx)),
      line_str, marker_str,
      to_pml(label_settings, show_label = !is.null(x$label_cols)),
      "<c:xVal>", to_pml(serie$x), "</c:xVal>",
      "<c:yVal>", to_pml(serie$y), "</c:yVal>",
      label_pml,
      sprintf("<c:smooth val=\"%.0f\"/>", serie$smooth),
      "</c:ser>"
    )
  },
  has_line = has_lines[x$options$scatterstyle],
  has_marker = has_markers[x$options$scatterstyle])

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:scatterChart>",
          sprintf("<c:scatterStyle val=\"%s\"/>", x$options$scatterstyle),
          sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
          str_series_,
          to_pml(x$label_settings, !is.null(x$label_cols)),
          x_ax_id, y_ax_id,
          "</c:scatterChart>"  )
}




#' @importFrom grDevices col2rgb
get_marker_xml <- function( fill, stroke, symbol = NULL, size = NULL){

  fill_elts <- col2rgb(fill, alpha = TRUE)[,1]
  fill_hex <- sprintf( "%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3]);
  stroke_elts <- col2rgb(stroke, alpha = TRUE)[,1]
  stroke_hex <- sprintf( "%02X%02X%02X", stroke_elts[1], stroke_elts[2], stroke_elts[3])

  str_sym <- ""
  if( !is.null(symbol) ){
    str_sym <- sprintf( "<c:symbol val=\"%s\"/>", symbol )
  }

  str_size <- ""
  if( !is.null(size) ){
    str_size <- sprintf( "<c:size val=\"%.0f\"/>", size )
  }

  paste0("<c:marker>",
         str_sym, str_size, get_sppr_xml(fill, stroke),
         "</c:marker>" )
}


get_sppr_xml <- function( fill, stroke, line_width = NULL){
  fill_elts <- col2rgb(fill, alpha = TRUE)[,1]
  fill_hex <- sprintf( "%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3]);
  stroke_elts <- col2rgb(stroke, alpha = TRUE)[,1]
  stroke_hex <- sprintf( "%02X%02X%02X", stroke_elts[1], stroke_elts[2], stroke_elts[3])
  stroke_width <- ""
  if(!is.null(line_width)){
    stroke_width <- sprintf(" w=\"%.0f\"", 12700*line_width)
  }
  paste0("<c:spPr>",
         sprintf("<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>", fill_hex,  fill_elts[4] / 255.0 * 100000 ),
         sprintf("<a:ln%s><a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill></a:ln>",
                 stroke_width, stroke_hex,  stroke_elts[4] / 255.0 * 100000 ),
         "<a:effectLst/></c:spPr>" )
}
get_sppr_xml_line_chart <- function( fill, stroke, style, width){
  fill_elts <- col2rgb(fill, alpha = TRUE)[,1]
  fill_hex <- sprintf( "%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3]);

  line_properties <- fp_border(color = stroke, style = style, width = width)
  line_str <- ooxml_fp_border(line_properties)

  paste0("<c:spPr>",
         sprintf("<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>", fill_hex,  fill_elts[4] / 255.0 * 100000 ),
         line_str,
         "</c:spPr>" )
}
