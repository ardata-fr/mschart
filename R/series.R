# str_ref ----
str_ref <- R6::R6Class(
  "str_ref",
  public = list(

    initialize = function( region, values ) {
      private$region <- region
      private$values <- values
    },

    pml = function(){
      pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
      pt_ <- sprintf(pt_, seq_along(private$values)-1, private$values)
      pt_ <- paste0(pt_, collapse = "")
      pml_ <- "<c:strRef><c:f>%s</c:f><c:strCache><c:ptCount val=\"%.0f\"/>%s</c:strCache></c:strRef>"
      sprintf(pml_, private$region, length(private$values), pt_)
    }

  ),
  private = list(
    region = NULL,
    values = NULL
  )

)
# num_ref ----
num_ref <- R6::R6Class(
  "num_ref",
  public = list(

    initialize = function( region, values ) {
      private$region <- region
      private$values <- values
    },

    pml = function(){
      pt_ <- "<c:pt idx=\"%.0f\"><c:v>%s</c:v></c:pt>"
      pt_ <- sprintf(pt_, seq_along(private$values)-1, private$values)
      pt_ <- paste0(pt_, collapse = "")
      pml_ <- "<c:numRef><c:f>%s</c:f><c:numCache><c:formatCode>General</c:formatCode><c:ptCount val=\"%.0f\"/>%s</c:numCache></c:numRef>"
      sprintf(pml_, private$region, length(private$values), pt_)
    }

  ),
  private = list(
    region = NULL,
    values = NULL
  )

)
# serie_barchart ----
serie_barchart <- R6::R6Class(
  "serie_barchart",
  public = list(

    initialize = function( idx, order, tx, cat, val ) {
      private$idx <- idx
      private$order <- order
      private$tx <- tx
      private$invert_if_negative <- '0'
      private$cat <- cat
      private$val <- val
    },
    pml = function(){
      str_ <- paste0("<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx>%s<c:cat>%s</c:cat><c:val>%s</c:val></c:ser>")
      str_ <- sprintf(str_,
                      private$idx, private$order,
                      private$tx$pml(),
                      "<c:invertIfNegative val=\"0\"/>",
                      private$cat$pml(),
                      private$val$pml()
      )
      str_
    }

  ),
  private = list(
    idx = NULL,
    order = NULL,
    tx = NULL,
    invert_if_negative = NULL,
    cat = NULL,
    val = NULL
  )

)

# serie_scatter ----
serie_scatter <- R6::R6Class(
  "serie_scatter",
  public = list(

    initialize = function( idx, order, tx, val_x, val_y, marker ) {
      private$idx <- idx
      private$order <- order
      private$tx <- tx
      private$marker <- marker
      private$val_x <- val_x
      private$val_y <- val_y
    },
    pml = function(){
      str_ <- paste0("<c:ser><c:idx val=\"%.0f\"/><c:order val=\"%.0f\"/><c:tx>%s</c:tx><c:xVal>%s</c:xVal><c:yVal>%s</c:yVal></c:ser>")
      str_ <- sprintf(str_,
                      private$idx, private$order,
                      private$tx$pml(),
                      #private$marker,
                      private$val_x$pml(),
                      private$val_y$pml()
      )
      str_
    }

  ),
  private = list(
    idx = NULL,
    order = NULL,
    tx = NULL,
    marker = NULL,
    val_x = NULL,
    val_y = NULL
  )

)

