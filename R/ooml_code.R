ooml_code <- function(x, id_x, id_y){
  UseMethod("ooml_code")
}

#' @importFrom purrr map_chr
ooml_code.ms_barchart <- function(x, id_x, id_y){

  series <- as_series(x, str_ref, num_ref)

  str_series_ <- map_chr( series, function(x, template ){
    sprintf(template, x$idx, x$order, x$tx$pml(),
            "<c:invertIfNegative val=\"0\"/>",
            x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
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
    sprintf(template, x$idx, x$order, x$tx$pml(), x$x$pml(), x$y$pml() )
  }, template = "<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx><c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
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

