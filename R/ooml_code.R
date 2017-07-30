ooml_code <- function(x, id_x, id_y){
  UseMethod("ooml_code")
}

#' @importFrom purrr map_chr
ooml_code.ms_barchart <- function(x, id_x, id_y){

  series <- as_series(x, str_ref, num_ref)

  str_series_ <- map_chr( series, function(x, template ){
    marker_str <- get_sppr_xml(x$fill, x$stroke )

    sprintf(template, x$idx, x$order, x$tx$pml(),
            marker_str,
            "<c:invertIfNegative val=\"0\"/>",
            x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
  str_series_ <- paste(str_series_, collapse = "")

  x_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_x)
  y_ax_id <- sprintf("<c:axId val=\"%s\"/>", id_y)

  str_ <- paste0( "<c:barChart>",
                  "<c:barDir val=\"%s\"/>",
                  "<c:grouping val=\"%s\"/>",
                  "<c:varyColors val=\"%.0f\"/>",
                  str_series_,
                  pml_labels_options(x$label_settings),
                  "<c:gapWidth val=\"%.0f\"/>",
                  "<c:overlap val=\"%.0f\"/>",
                  x_ax_id, y_ax_id,
                  "</c:barChart>"  )

  sprintf(str_, x$options$dir, x$options$grouping,
          x$options$vary_colors, x$options$gap_width,
          x$options$overlap)

}

#' @importFrom purrr map_chr
ooml_code.ms_linechart <- function(x, id_x, id_y){

  series <- as_series(x, num_ref, num_ref)

  str_series_ <- map_chr( series, function(x, template ){
    marker_str <- get_marker_xml(x$fill, x$stroke, x$symbol, x$size )
    sppr_str <- get_sppr_xml(x$fill, x$stroke )

    sprintf(template, x$idx, x$order, x$tx$pml(), sppr_str, marker_str, x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
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


#' @importFrom purrr map_chr
ooml_code.ms_areachart <- function(x, id_x, id_y){

  series <- as_series(x, num_ref, num_ref)

  str_series_ <- map_chr( series, function(x, template ){
    marker_str <- get_sppr_xml(x$fill, x$stroke )

    sprintf(template, x$idx, x$order, x$tx$pml(), marker_str, x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
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

#' @importFrom purrr map_chr
ooml_code.ms_scatterchart <- function(x, id_x, id_y){

  series <- as_series(x, num_ref, num_ref)

  str_series_ <- map_chr( series, function(x, template ){
    marker_str <- get_marker_xml(x$fill, x$stroke, x$symbol, x$size )

    sprintf(template, x$idx, x$order, x$tx$pml(),
            marker_str,
            x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx><c:spPr><a:ln><a:noFill/></a:ln></c:spPr>%s<c:xVal>%s</c:xVal><c:yVal>%s</c:yVal></c:ser>")
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
