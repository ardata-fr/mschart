#' @importFrom grDevices colors
ms_chart <- function(data, x, y, group = NULL){

  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  if( !is.null(group) && !(group %in% names(data)) ){
    stop("column ", shQuote(group), " could not be found in data.", call. = FALSE)
  }

  theme_ <- mschart_theme()

  tryCatch(
    x_axis_tag <- get_axis_tag(data[[x]]),
    error = function(e) {
      stop("column ", shQuote(x), ": ", e$message, " [", paste(class(data[[x]]), collapse = ","), "]", call. = FALSE)
    })
  tryCatch(
    y_axis_tag <- get_axis_tag(data[[y]]),
    error = function(e) {
      stop("column ", shQuote(y), ": ", e$message, " [", paste(class(data[[y]]), collapse = ","), "]", call. = FALSE)
    })

  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")


  lbls <- list(title = NULL, x = x, y = y)

  out <- list(data = data, x = x, y = y, group = group,
              theme = theme_,
              options = list(),
              x_axis = x_axis_,
              y_axis = y_axis_,
              axis_tag = list(x = x_axis_tag,
                              y = y_axis_tag),
              fmt_names = list( x = fmt_name(data[[x]]),
                                y = fmt_name(data[[y]]) ),
              labels = lbls)
  class(out) <- c("ms_chart")
  out <- chart_data_labels(out)
  out$data_series <- shape_as_series(out)

  series_names <- setdiff(colnames( out$data_series ), x )

  if( length(series_names) <= length(colour_list) )
    palette_ <- colour_list[[length(series_names)]]
  else
    palette_ <- sample(colors(), size = length(series_names), replace = TRUE)

  series_symbols <- rep("circle", length(series_names) )
  series_size <- rep(12, length(series_names) )
  out$series_settings <- list( fill = setNames(palette_, series_names),
        colour = setNames(palette_, series_names),
        symbol = setNames(series_symbols, series_names),
        size = setNames(series_size, series_names)
        )
  out
}

#' ms_chart print method
#'
#' @param x ms_chart object
#' @param preview preview the chart in a PowerPoint document
#' @param ... unused
#' @export
#' @importFrom officer read_pptx add_slide
#' @importFrom utils browseURL
print.ms_chart <- function(x, preview = FALSE, ...){

  if( preview && interactive() ){
      doc <- read_pptx()
      doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
      doc <- ph_with_chart(doc, chart = x)
      file_out <- print(doc, target = tempfile(fileext = ".pptx"))
      browseURL(file_out)
      return(invisible())
  }
  class_val <- setdiff( class(x), "ms_chart" )
  cat( sprintf("* %s object\n\n", shQuote(class_val)) )

  cat(sprintf("* original data [%.0f,%.0f] (sample):\n", nrow(x$data), ncol(x$data)))
  print( x$data[ seq_len( min(c( nrow(x$data), 5)) ), ] )
  cat(sprintf("\n* series data [%.0f,%.0f] (sample):\n", nrow(x$data_series), ncol(x$data_series)))
  print( x$data_series[ seq_len( min(c( nrow(x$data_series), 5))), ] )

}

colour_list <- list(
   c("#4477AA"),
   c("#4477AA", "#CC6677"),
   c("#4477AA", "#DDCC77", "#CC6677"),
   c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
   c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
   c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499"),
   c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499"),
   c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499"),
   c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
   c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
   c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
   c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
)


#' @importFrom xml2 xml_attr<-
format.ms_chart  <- function(x, id_x, id_y){
  str_ <- ooml_code(x, id_x = id_x, id_y = id_y)


  if( is.null(x$x_axis$num_fmt) )
    x$x_axis$num_fmt <- x$theme[[x$fmt_names$x]]
  if( is.null(x$y_axis$num_fmt) )
    x$y_axis$num_fmt <- x$theme[[x$fmt_names$y]]

  axes_str <- axes_xml(x, id_x = id_x, id_y = id_y)
  ns <- "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""
  xml_elt <- paste0("<c:plotArea ", ns, "><c:layout/>", str_, axes_str, "</c:plotArea>")

  xml_doc <- read_xml(system.file(package = "mschart", "template", "chart.xml"))

  node <- xml_find_first(xml_doc, "//c:plotArea")
  xml_replace( node, as_xml_document(xml_elt) )

  if( !is.null( x$labels[["title"]] ) ){
    chartnode <- xml_find_first(xml_doc, "//c:chart")
    title_ <- "<c:title %s><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, ns, format(x$theme[["main_title"]], type = "pml" ), x$labels[["title"]] )
    xml_add_child( chartnode, as_xml_document(title_), .where	= 0 )
  }
  legend_pos <- xml_find_first(xml_doc, "//c:chart/c:legend/c:legendPos")
  xml_attr( legend_pos, "val" ) <- x$theme[["legend_position"]]

  as.character(xml_doc)
}


#' @describeIn mschart line plot
#' @export
#' @examples
#' library(officer)
#'
#'
#' ##########################
#' # linecharts example -----
#' ##########################
#'
#' @example examples/02_linechart.R
ms_linechart <- function(data, x, y, group = NULL){
  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- linechart_options()
  class(out) <- c("ms_linechart", "ms_chart")

  serie_names <- names(out$series_settings$symbol)
  values <- setNames( rep( "none", length(serie_names)), serie_names )
  out <- chart_data_symbol(out, values = values)

  out
}

#' @describeIn mschart bar plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # barcharts example -----
#' ##########################
#'
#' @example examples/01_barchart.R
ms_barchart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  out$options <- barchart_options()
  class(out) <- c("ms_barchart", "ms_chart")
  out
}

#' @describeIn mschart area plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # areacharts example -----
#' ##########################
#'
#' @example examples/03_areachart.R
ms_areachart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  class(out) <- c("ms_areachart", "ms_chart")
  out <- chart_settings(out)

  serie_names <- names(out$series_settings$colour)
  values <- setNames( rep( "transparent", length(serie_names)), serie_names )
  out <- chart_data_stroke(out, values = values)

  out
}

#' @describeIn mschart scatter plot
#' @export
#' @examples
#'
#'
#' ##########################
#' # scattercharts example -----
#' ##########################
#'
#' @example examples/04_scatterchart.R
ms_scatterchart <- function(data, x, y, group = NULL){

  out <- ms_chart(data = data, x = x, y = y, group = group)
  class(out) <- c("ms_scatterchart", "ms_chart")

  out <- chart_settings(out)
  out <- pretty_num_axes(out)

  out
}

#' @export
#' @title Modify axis and plot labels
#' @description Add labels to a chart, labels can be specified for
#' x axis, y axis and plot.
#' @param x chart object
#' @param title,xlab,ylab Text to add
#' @examples
#' mylc <- ms_linechart(data = browser_ts, x = "date", y = "freq",
#'   group = "browser")
#' mylc <- chart_labels(mylc, title = "my title", xlab = "my x label",
#'   ylab = "my y label")
chart_labels <- function( x, title = NULL, xlab = NULL, ylab = NULL){
  if( !is.null(title) ) x$labels[["title"]] <- title
  if( !is.null(xlab) ) x$labels[["x"]] <- xlab
  if( !is.null(ylab) ) x$labels[["y"]] <- ylab
  x
}

