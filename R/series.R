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

label_ref <- function(values, region = NULL, num_fmt = NULL){
  x <- list(
    region = region,
    values = values,
    num_fmt = num_fmt
  )
  class(x) <- c("col_ref", "label_ref")
  x
}

to_pml.label_ref <- function(x, add_ns = FALSE, ...){

  values <- character(length(x$values))

  if( is.factor(x$values) ){
    values <- htmlEscape(as.character(x$values))
  } else if( is.numeric(x$values) ){
    values <- as.character(x$values)
  } else if( is.character(x$values) ){
    values <- htmlEscape(x$values)
  } else htmlEscape(format(x$values))

  pt_ <- sprintf("<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>", seq_along(values)-1, values)
  pt_ <- paste0(pt_, collapse = "")

  num_fmt <- ""
  pml_ <- "<c15:datalabelsRange><c15:f>%s</c15:f><c15:dlblRangeCache>%s<c:ptCount val=\"%.0f\"/>%s</c15:dlblRangeCache></c15:datalabelsRange>"
  pml_ <- paste0(
    "<c:extLst>",
    "<c:ext uri=\"{02D57815-91ED-43cb-92C2-25804820EDAC}\" xmlns:c15=\"http://schemas.microsoft.com/office/drawing/2012/chart\">",
    pml_, "</c:ext></c:extLst>")

  sprintf(pml_, x$region, num_fmt, length(values), pt_)
}

