#' @importFrom stats as.formula
#' @importFrom data.table as.data.table dcast.data.table setorderv
shape_as_series <- function(x){
  dataset <- as.data.table(x$data)
  dataset <- setorderv(dataset, c(x$x, x$group) )

  if( !is.null(x$group)){
    form_str <- sprintf("%s ~ %s", x$x, x$group)
    dataset <- dcast.data.table(dataset, formula = as.formula(form_str),
                                fun.aggregate = function(x) {x},
                                fill = NA, value.var = x$y )
  }
  as.data.frame(dataset)
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

