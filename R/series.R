#' @importFrom officer to_pml
str_ref <- function(values, region = NULL, num_fmt = NULL){
  x <- list(
    region = region,
    values = values,
    num_fmt = num_fmt
  )
  class(x) <- c("col_ref", "str_ref")
  x
}

#' @importFrom stats update
update.col_ref <- function(object, values = NULL, region = NULL, num_fmt = NULL, ...){
  if(!is.null(values)){
    object$values <- values
  }
  if(!is.null(region)){
    object$region <- region
  }
  if(!is.null(num_fmt)){
    object$num_fmt <- num_fmt
  }
  object
}


to_pml.str_ref <- function(x, add_ns = FALSE, ...){

  pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
  values <- character(length(x$values))

  if( is.factor(x$values) ){
    values <- htmlEscape(as.character(x$values))
  } else if( is.numeric(x$values) ){
    values <- as.character(x$values)
  } else if( is.character(x$values) ){
    values <- htmlEscape(x$values)
  }

  pt_ <- sprintf(pt_, seq_along(values)-1, values)
  pt_ <- paste0(pt_, collapse = "")

  num_fmt <- ""
  if( !is.null(x$num_fmt) )
    num_fmt <- sprintf("<c:formatCode>%s</c:formatCode>", x$num_fmt)
  pml_ <- "<c:strRef><c:f>%s</c:f><c:strCache>%s<c:ptCount val=\"%.0f\"/>%s</c:strCache></c:strRef>"

  sprintf(pml_, x$region, num_fmt, length(values), pt_)
}


num_ref <- function(values, region = NULL, num_fmt = NULL){
  x <- list(
    region = region,
    values = values,
    num_fmt = num_fmt
  )
  class(x) <- c("col_ref", "num_ref")
  x
}

to_pml.num_ref <- function(x, add_ns = FALSE, ...){
  pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
  values <- character(length(x$values))
  if( inherits(x$values, "Date") ){
    values <- as.integer(x$values - as.Date("1900-01-01") - 2)
  } else {
    values <- format(x$values, trim = TRUE, scientific = FALSE,
                     big.mark = "",
                     decimal.mark = ".")
  }

  pt_ <- sprintf(pt_, seq_along(values)-1, values)
  pt_ <- paste0(pt_[!is.na(x$values)], collapse = "")
  num_fmt <- ""
  if( !is.null(x$num_fmt) )
    num_fmt <- sprintf("<c:formatCode>%s</c:formatCode>", x$num_fmt )
  pml_ <- "<c:numRef><c:f>%s</c:f><c:numCache>%s<c:ptCount val=\"%.0f\"/>%s</c:numCache></c:numRef>"
  sprintf(pml_, x$region, num_fmt, length(values), pt_)
}

date_ref <- function(values, region = NULL, num_fmt = NULL){
  x <- list(
    region = region,
    values = values,
    num_fmt = num_fmt
  )
  class(x) <- c("col_ref", "date_ref")
  x
}

to_pml.date_ref <- function(x, add_ns = FALSE, ...){
  pt_ <- "<c:pt idx=\"%.0f\"><c:v>%.0f</c:v></c:pt>"
  values <- as.integer(x$values - as.Date("1900-01-01") - 2)
  pt_ <- sprintf(pt_, seq_along(values)-1, values)
  pt_ <- paste0(pt_[!is.na(values)], collapse = "")
  num_fmt <- sprintf("<c:formatCode>%s</c:formatCode>", "yyyy\\-mm\\-dd" )
  pml_ <- "<c:numRef><c:f>%s</c:f><c:numCache>%s<c:ptCount val=\"%.0f\"/>%s</c:numCache></c:numRef>"
  sprintf(pml_, x$region, num_fmt, length(values), pt_)
}



# as_series ----
#' @importFrom cellranger cell_limits as.range ra_ref to_string
as_series <- function(x, x_class, y_class, sheetname = "sheet1" ){
  dataset <- x$data_series
  w_x <- which( names(dataset) %in% x$x )

  x_serie_range <- cell_limits(ul = c(2, w_x),
                               lr = c(nrow(dataset)+1, w_x),
                               sheet = sheetname)
  x_serie_range <- as.range(x_serie_range, fo = "A1", strict = TRUE, sheet = TRUE)
  x_serie <- update(x_class, region = x_serie_range, values = dataset[[x$x]])

  series <- list()
  w_y_values <- which( names(dataset) %in% setdiff(names(dataset), x$x) )

  for( w_y in w_y_values){
    y_colname <- names(dataset)[w_y]

    serie_name_range <- ra_ref(row_ref = 1, col_ref = w_y, sheet = sheetname)
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    serie_name <- str_ref(values = y_colname, region = serie_name_range)

    y_serie_range <- cell_limits(ul = c(2, w_y), lr = c(nrow(dataset)+1, w_y),  sheet = sheetname)
    y_serie_range <- as.range(y_serie_range, fo = "A1", strict = TRUE, sheet = TRUE)

    y_serie <- update(y_class, region = y_serie_range, values = dataset[[y_colname]])

    ser <- list( idx = length(series), order = length(series),
                 tx = serie_name,
                 x = x_serie, y = y_serie,
                 stroke = x$series_settings$colour[y_colname],
                 fill = x$series_settings$fill[y_colname],
                 symbol = x$series_settings$symbol[y_colname],
                 line_style = x$series_settings$line_style[y_colname],
                 size = x$series_settings$size[y_colname],
                 line_width = x$series_settings$line_width[y_colname],
                 labels_fp = x$series_settings$labels_fp[[y_colname]],
                 smooth = x$series_settings$smooth[y_colname]
                 )
    series <- append(series, list(ser) )
  }
  series
}
