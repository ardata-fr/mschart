#' @importFrom stats as.formula
#' @importFrom data.table as.data.table dcast.data.table setorderv
#' @importFrom htmltools htmlEscape
shape_as_series <- function(x){
  dataset <- as.data.table(x$data)
  dataset <- setorderv(dataset, c(x$x, x$group) )

  if( !is.null(x$group)){
    dataset[[x$group]] <- htmlEscape(dataset[[x$group]])
    form_str <- sprintf("%s ~ %s", x$x, x$group)
    dataset <- dcast.data.table(dataset, formula = as.formula(form_str),
                                fun.aggregate = function(x) {x},
                                fill = NA, value.var = x$y )
  } else {
    dataset <- dataset[, c(x$x, x$y), with = FALSE ]
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


