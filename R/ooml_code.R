ooml_code <- function(x, id_x, id_y, sheetname = "sheet1"){
  UseMethod("ooml_code")
}

clustered_pos <- c("ctr", "inBase", "inEnd", "outEnd")
stacked_pos <- c("ctr", "inBase", "inEnd")

ooml_code.ms_barchart <- function(x, id_x, id_y, sheetname = "sheet1"){

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
    marker_str <- get_sppr_xml(serie$fill, serie$stroke )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp
    labels_ooxml <- pml_labels_options(label_settings)

    sprintf(template, serie$idx, serie$order, serie$tx$pml(),
            marker_str,
            "<c:invertIfNegative val=\"0\"/>",
            labels_ooxml,
            serie$x$pml(), serie$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
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
                  pml_labels_options(x$label_settings),
                  sprintf("<c:gapWidth val=\"%.0f\"/>", x$options$gap_width),
                  sprintf("<c:overlap val=\"%.0f\"/>", x$options$overlap),
                  x_ax_id, y_ax_id,
                  "</c:barChart>" )
}

standard_pos <- c("b", "ctr", "l", "r", "t")
ooml_code.ms_linechart <- function(x, id_x, id_y, sheetname = "sheet1"){

  if( !x$label_settings$position %in% standard_pos ){
    stop("label position issue.",
         "Arg. position in chart_data_labels() should match one of ",
         paste(shQuote(standard_pos), collapse = ", "), ".", call. = FALSE)
  }

  template_str <- paste0("<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s%s",
                     "<c:cat>%s</c:cat>",
                     "<c:val>%s</c:val></c:ser>")
  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]), sheetname = sheetname )
  str_series_ <- sapply( series, function(serie, template ){
    marker_str <- get_marker_xml(serie$fill, serie$stroke, serie$symbol, serie$size )
    sppr_str <- get_sppr_xml(serie$fill, serie$stroke )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp
    labels_ooxml <- pml_labels_options(label_settings)


    sprintf(template, serie$idx, serie$order, serie$tx$pml(), sppr_str, marker_str,
            labels_ooxml,
            serie$x$pml(), serie$y$pml() )
  }, template = template_str)
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:lineChart>",
                  sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
                  sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
                  str_series_,
                  pml_labels_options(x$label_settings),
                  x_ax_id, y_ax_id,
                  "</c:lineChart>"  )
}


ooml_code.ms_areachart <- function(x, id_x, id_y, sheetname = "sheet1"){

  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]), sheetname = sheetname )

  str_series_ <- sapply( series, function(serie, template ){
    marker_str <- get_sppr_xml(serie$fill, serie$stroke )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp
    labels_ooxml <- pml_labels_options(label_settings)

    sprintf(template, serie$idx, serie$order, serie$tx$pml(), marker_str, labels_ooxml, serie$x$pml(), serie$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:areaChart>",
          sprintf("<c:grouping val=\"%s\"/>", x$options$grouping),
          sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
          str_series_,
          pml_labels_options(x$label_settings),
          x_ax_id, y_ax_id,
          "</c:areaChart>"  )
}

scatterstyles <- c('none', 'line', 'lineMarker', 'marker', 'smooth', 'smoothMarker')
has_markers <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
names(has_markers) <- scatterstyles
has_lines <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
names(has_lines) <- scatterstyles

ooml_code.ms_scatterchart <- function(x, id_x, id_y, sheetname = "sheet1"){

  if( !x$label_settings$position %in% standard_pos ){
    stop("label position issue.",
         "Arg. position in chart_data_labels() should match one of ",
         paste(shQuote(standard_pos), collapse = ", "), ".", call. = FALSE)
  }


  series <- as_series(x, x_class = serie_builtin_class(x$data[[x$x]]),
                      y_class = serie_builtin_class(x$data[[x$y]]),
                      sheetname = sheetname )

  str_series_ <- sapply( series, function(serie, template, has_line, has_marker ){

    if( !has_line ){
      line_str <- "<c:spPr><a:ln><a:noFill/></a:ln></c:spPr>"
    } else {
      line_properties <- fp_border(color = serie$stroke, style = "solid", width = serie$line_width)
      line_str <- ooxml_fp_border(line_properties,
                      in_tags = c("c:spPr"))
    }
    if( !has_marker )
      marker_str <- "<c:marker><c:symbol val=\"none\"/></c:marker>"
    else marker_str <- get_marker_xml(serie$fill, serie$stroke, serie$symbol, serie$size )

    label_settings <- x$label_settings
    label_settings$labels_fp <- serie$labels_fp
    labels_ooxml <- pml_labels_options(label_settings)

    sprintf(template, serie$idx, serie$order, serie$tx$pml(),
            line_str, marker_str,
            labels_ooxml,
            serie$x$pml(), serie$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s%s<c:xVal>%s</c:xVal><c:yVal>%s</c:yVal></c:ser>",
  has_line = has_lines[x$options$scatterstyle],
  has_marker = has_markers[x$options$scatterstyle])

  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  paste0( "<c:scatterChart>",
          sprintf("<c:scatterStyle val=\"%s\"/>", x$options$scatterstyle),
          sprintf("<c:varyColors val=\"%.0f\"/>", x$options$vary_colors),
          str_series_,
          pml_labels_options(x$label_settings),
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


get_sppr_xml <- function( fill, stroke){
  fill_elts <- col2rgb(fill, alpha = TRUE)[,1]
  fill_hex <- sprintf( "%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3]);
  stroke_elts <- col2rgb(stroke, alpha = TRUE)[,1]
  stroke_hex <- sprintf( "%02X%02X%02X", stroke_elts[1], stroke_elts[2], stroke_elts[3])

  paste0("<c:spPr>",
         sprintf("<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>", fill_hex,  fill_elts[4] / 255.0 * 100000 ),
         sprintf("<a:ln><a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill></a:ln>", stroke_hex,  stroke_elts[4] / 255.0 * 100000 ),
         "</c:spPr>" )
}
