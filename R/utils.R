#' @importFrom stats as.formula
#' @importFrom data.table as.data.table dcast.data.table setorderv
shape_as_series <- function(x){
  dataset <- as.data.table(x$data)

  if( !is.null(x$group)){
    dataset <- groupify_data(x, dataset)
  } else {
    # Happens by reference, no need to assign
    setorderv(dataset, c(x$x))
    dataset <- dataset[, c(x$x, x$y), with = FALSE ]
  }

  as.data.frame(dataset)
}

#' @importFrom data.table as.data.table
groupify_data <- function(x, dataset) {
  # This function relies on a grouop being defined
  stopifnot(!is.null(x$group))

  # This weird looking line is designed to resolve the notes that get generated
  # during devtools::check() because .N and := aren't explicitly made visible
  `:=` <- .N <- NULL

  unique_rows_dataset <- unique(dataset[, c(x$x, x$y, x$group), with=F])

  dummy_column_name <- generate_temp_column_name(unique_rows_dataset)
  unique_rows_dataset[, (dummy_column_name):=seq(.N), keyby=c(x$x, x$group)]

  ds <- dcast.data.table(unique_rows_dataset,
                         formula = as.formula(sprintf("%s + %s ~ %s", x$x, dummy_column_name, x$group)),
                         value.var = x$y, fill = NA)

  ds[, (dummy_column_name) := NULL]
  ds
}

generate_temp_column_name <- function(tbl) {
  paste0(sample(letters, max(sapply(names(tbl), nchar)+1), replace = T), collapse = '')
}

#' @importFrom htmltools htmlEscape
class_preserving_html_escape <- function(text, attribute = FALSE) {
  if (is.factor(text)) {
    factor(text, levels=htmltools::htmlEscape(levels(text)))
  } else {
    htmltools::htmlEscape(text)
  }
}

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
    str_ref
  else if( is.factor(x) || is.character(x) )
    str_ref
  else if( is.integer(x) )
    num_ref
  else if( is.double(x) )
    num_ref
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
