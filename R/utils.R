fmt_name <- function( x ){

  if( inherits(x, "Date") )
    "date_fmt"
  else if( is.factor(x) || is.character(x) )
    "str_fmt"
  else if( is.integer(x) )
    "integer_fmt"
  else if( is.double(x) )
    "double_fmt"
  else stop("unknow type of data")

  x
}

serie_builtin_class <- function( x ){

  if( inherits(x, "Date") )
    date_ref(x)
  else if( is.factor(x) || is.character(x) )
    str_ref(x)
  else if( is.integer(x) )
    num_ref(x)
  else if( is.double(x) )
    num_ref(x)
  else stop("unknow type of data")
}

is_valid_color = function(x) {
  sapply(x, function( x ) {
    tryCatch( is.matrix( col2rgb( x ) ), error = function( e ) FALSE )
  })
}

pretty_num_axes <- function(x){
  range_x <- pretty(x$data[[x$x]])
  range_y <- pretty(x$data[[x$y]])
  x <- chart_ax_x(x, limit_min = range_x[1], limit_max = range_x[length(range_x)])
  x <- chart_ax_y(x, limit_min = range_y[1], limit_max = range_y[length(range_y)])
  x
}

#' @importFrom grDevices rgb
ooxml_fp_border <- function(x, in_tags = NULL ){
  stopifnot(inherits(x, "fp_border"))
  colspecs <- as.list(col2rgb( x$color, alpha = TRUE )[,1] / 255)

  alpha <- colspecs$alpha
  is_transparent <- alpha < .0001

  if( is_transparent || x$width < 0.001 || x$style %in% "none" ){
    return("")
  }

  colspecs$alpha <- NULL
  hexcol <- do.call(rgb,colspecs)
  hexcol <- substring(hexcol, 2 )

  solidfill <- "<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>"
  solidfill <- sprintf(solidfill, hexcol, alpha * 100000)

  presetdash <- "<a:prstDash val="
  if( x$style %in% "solid" ){
    presetdash <- paste0(presetdash, "\"solid\"/>")
  } else if( x$style %in% "dotted" ){
    presetdash <- paste0(presetdash, "\"sysDot\"/>")
  } else {
    presetdash <- paste0(presetdash, "\"sysDash\"/>")
  }

  out <- sprintf("<a:ln algn=\"ctr\" w=\"%.0f\">", x$width * 12700)
  out <- paste0(out, solidfill, presetdash, "</a:ln>")

  if( !is.null(in_tags) ){
    begin <- paste0("<", in_tags, ">", collapse = "")
    end <- paste0("</", rev(in_tags), ">", collapse = "")
    out <- paste0(begin, out, end)
  }
  out
}


ooxml_txpr <- function( fptext ){
  out <- format(fptext, type = "pml" )
  out <- gsub("a:rPr", "a:defRPr", out, fixed = TRUE)
  rpr <- "<a:p><a:pPr>%s</a:pPr></a:p>"
  rpr <- sprintf(rpr, out)
  paste0("<c:txPr><a:bodyPr/><a:lstStyle/>", rpr, "</c:txPr>")
}
