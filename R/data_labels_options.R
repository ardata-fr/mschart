#' @export
data_labels_options <- function(num_fmt = "General", position = "ctr",
                                show_legend_key = FALSE, show_val = FALSE,
                                show_cat_name = FALSE, show_serie_name = FALSE,
                                show_percent = FALSE, separator = "\n" ){

  if( !position %in% st_dlblpos ){
    stop("position should be one of ", paste0(shQuote(st_dlblpos), collapse = ", " ))
  }

  out <- list(num_fmt = num_fmt, position = position,
       show_legend_key = show_legend_key, show_val = show_val,
       show_cat_name = show_cat_name, show_serie_name = show_serie_name,
       show_percent = show_percent, separator = separator )
  class(out) <- "labels_options"
  out

}

pml_labels_options <- function(x){
  str_ <- paste0("<c:dLbls>",
         sprintf("<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>", x$num_fmt),
         sprintf("<c:dLblPos val=\"%s\"/>", x$position),
         sprintf("<c:showLegendKey val=\"%.0f\"/>", x$show_legend_key),
         sprintf("<c:showVal val=\"%.0f\"/>", as.integer(x$show_val)),
         sprintf("<c:showCatName val=\"%.0f\"/>", as.integer(x$show_cat_name)),
         sprintf("<c:showSerName val=\"%.0f\"/>", as.integer(x$show_serie_name)),
         sprintf("<c:showPercent val=\"%.0f\"/>", x$show_percent),
         sprintf("<c:showBubbleSize val=\"%.0f\"/>", FALSE),
         sprintf("<c:separator val=\"%s\"/>", x$separator),
         "</c:dLbls>")

  str_

}

