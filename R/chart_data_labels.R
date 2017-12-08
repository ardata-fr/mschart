#' @export
#' @title Modify data labels settings
#' @description Data labels show details about data series.
#' @param x an \code{ms_chart} object.
#' @param num_fmt `character(1)`: number formatting specifies number format properties which
#' indicate how to format and render the numeric values. It can be "General", "0.00", "#,##0",
#' "#,##0.00", "mm-dd-yy", "m/d/yy h:mm", etc.
#' @param position `character(1)`: it specifies the position of the data label.
#' It should be one of 'b', 'ctr', 'inBase', 'inEnd', 'l',
#' 'outEnd', 'r', 't'.
#' When grouping is 'clustered', it should be one of 'ctr','inBase','inEnd','outEnd'.
#' When grouping is 'stacked', it should be one of 'ctr','inBase','inEnd'.
#' When grouping is 'standard', it should be one of 'b','ctr','l','r','t'.
#' @param show_legend_key show legend key if TRUE.
#' @param show_val show values if TRUE.
#' @param show_cat_name show categories if TRUE.
#' @param show_serie_name show names of series if TRUE.
#' @param show_percent show percentages if TRUE.
#' @param separator separator for displayed labels.
chart_data_labels <- function(x, num_fmt = "General", position = "ctr",
                                show_legend_key = FALSE, show_val = FALSE,
                                show_cat_name = FALSE, show_serie_name = FALSE,
                                show_percent = FALSE, separator = ", " ){

  if( !position %in% st_dlblpos ){
    stop("position should be one of ", paste0(shQuote(st_dlblpos), collapse = ", " ))
  }

  out <- list(num_fmt = num_fmt, position = position,
       show_legend_key = show_legend_key, show_val = show_val,
       show_cat_name = show_cat_name, show_serie_name = show_serie_name,
       show_percent = show_percent, separator = separator )
  class(out) <- "labels_options"
  x$label_settings <- out
  x
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

