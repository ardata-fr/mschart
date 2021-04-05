#' @importFrom R6 R6Class
# str_ref ----
str_ref <- R6::R6Class(
  "str_ref",
  public = list(

    initialize = function( region, values, num_fmt = NULL ) {
      private$region <- region
      private$values <- values
      private$num_fmt <- num_fmt
    },

    pml = function(){
      pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
      if( inherits(private$values, "Date") ){
        pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
        private$values <- format(private$values)
      } else if( is.factor(private$values) ){
        private$values <- htmlEscape(as.character(private$values))
      } else if( is.numeric(private$values) ){
        private$values <- as.character(private$values)
      } else if( is.character(private$values) ){
        private$values <- htmlEscape(private$values)
      }
      pt_ <- sprintf(pt_, seq_along(private$values)-1, private$values)
      pt_ <- paste0(pt_, collapse = "")
      num_fmt <- ""
      if( !is.null(private$num_fmt) )
        num_fmt <- sprintf("<c:formatCode>%s</c:formatCode>", private$num_fmt )
      pml_ <- "<c:strRef><c:f>%s</c:f><c:strCache>%s<c:ptCount val=\"%.0f\"/>%s</c:strCache></c:strRef>"
      sprintf(pml_, private$region, num_fmt, length(private$values), pt_)
    }

  ),
  private = list(
    region = NULL,
    values = NULL,
    num_fmt = NULL
  )
)

# num_ref ----
num_ref <- R6::R6Class(
  "num_ref",
  public = list(

    initialize = function( region, values, num_fmt = NULL ) {
      private$region <- region
      private$values <- values
      private$num_fmt <- num_fmt
    },

    pml = function(){
      pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
      if( inherits(private$values, "Date") ){
        private$values <- as.integer(private$values - as.Date("1900-01-01") - 2)
      }
      pt_ <- sprintf(pt_, seq_along(private$values)-1, private$values)
      pt_ <- paste0(pt_[!is.na(private$values)], collapse = "")
      num_fmt <- ""
      if( !is.null(private$num_fmt) )
        num_fmt <- sprintf("<c:formatCode>%s</c:formatCode>", private$num_fmt )
      pml_ <- "<c:numRef><c:f>%s</c:f><c:numCache>%s<c:ptCount val=\"%.0f\"/>%s</c:numCache></c:numRef>"
      sprintf(pml_, private$region, num_fmt, length(private$values), pt_)
    }

  ),
  private = list(
    region = NULL,
    values = NULL,
    num_fmt = NULL
  )

)


# as_series ----
#' @importFrom cellranger cell_limits as.range ra_ref to_string
as_series <- function(x, x_class, y_class, sheetname = "sheet1" ){
  dataset <- x$data_series

  w_x <- which( names(dataset) %in% x$x )
  x_serie_range <- cell_limits(ul = c(2, w_x),
                               lr = c(nrow(dataset)+1, w_x),
                               sheet = sheetname)
  x_serie_range <- as.range(x_serie_range, fo = "A1", strict = TRUE, sheet = TRUE)
  x_serie <- x_class$new( x_serie_range, dataset[[x$x]] )

  series <- list()

  w_y_values <- which( names(dataset) %in% setdiff(names(dataset), x$x) )

  for( w_y in w_y_values){

    y_colname <- names(dataset)[w_y]

    serie_name_range <- ra_ref(row_ref = 1, col_ref = w_y, sheet = sheetname)
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    serie_name <- str_ref$new( serie_name_range, y_colname )

    y_serie_range <- cell_limits(ul = c(2, w_y), lr = c(nrow(dataset)+1, w_y),  sheet = sheetname)
    y_serie_range <- as.range(y_serie_range, fo = "A1", strict = TRUE, sheet = TRUE)
    y_serie <- y_class$new( y_serie_range, dataset[[y_colname]] )
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
