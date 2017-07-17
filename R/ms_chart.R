#' @importFrom cellranger ra_ref
#' @importFrom purrr map_chr
#' @importFrom R6 R6Class



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
# serie_data ----
serie_data <- R6::R6Class(
  "serie_data",
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




# ms_barchart ----
#' @export
ms_barchart <- R6::R6Class(
  "ms_barchart",
  public = list(

    initialize = function( dataset, dir, grouping, vary_colors = "0", gap_width = 150 ) {
      private$dataset <- dataset
      private$dir <- dir
      private$grouping <- grouping
      private$vary_colors <- vary_colors
      private$series <- list()
      private$gap_width <- gap_width
    },

    get_data = function(){
      private$dataset
    },

    add_serie = function( cat_col, val_col ){
      dataset <- private$dataset
      w_cat <- which( names(dataset) %in% cat_col )
      w_val <- which( names(dataset) %in% val_col )

      serie_name_range <- cellranger::ra_ref(row_ref = 1, col_ref = w_val, sheet = "sheet1")
      serie_name_range <- cellranger::to_string(serie_name_range, fo = "A1")
      serie_name_ref_ <- str_ref$new( serie_name_range, val_col )

      serie_value_range <- cellranger::cell_limits(ul = c(2, w_val), lr = c(nrow(dataset)+1, w_val),  sheet = "sheet1")
      serie_value_range <- cellranger::as.range(serie_value_range, fo = "A1", strict = TRUE, sheet = TRUE)
      serie_values_ <- num_ref$new( serie_value_range, dataset[[val_col]] )

      serie_cat_range <- cellranger::cell_limits(ul = c(2, w_cat),
                                     lr = c(nrow(dataset)+1, w_cat),
                                     sheet = "sheet1")
      serie_cat_range <- cellranger::as.range(serie_cat_range, fo = "A1", strict = TRUE, sheet = TRUE)
      serie_cat_ <- str_ref$new( serie_cat_range, dataset[[cat_col]] )

      ser <- serie_data$new( idx = length(private$series),
                      order = length(private$series),
                      tx = serie_name_ref_,
                      cat = serie_cat_, val = serie_values_ )
      private$series <- append(private$series, list(ser) )
      self
    },

    set_scale_x_discrete = function(orientation="minMax",
                                    type = "solid", width = 1, colour = "black"){
      cat_ax <- cat_axis$new(id="64451712", orientation=orientation,
                             axis_position="b",
                             cross_ax="64453248")
      cat_ax <- cat_ax$set_major_grid_lines(value = sp_pr$new(type = type, width = width, colour = colour))
      private$x_axis <- cat_ax
      self
    },
    set_scale_y_continuous = function(orientation="minMax",
                                    type = "solid", width = 1, colour = "black"){
      valax <- val_axis$new(id="64453248",
                            orientation="minMax", axis_position="l",
                            cross_ax="64451712")
      valax <- valax$set_major_grid_lines()

      private$y_axis <- valax
      self
    },

    pml = function(){
      str_series_ <- map_chr(private$series, function(x) x$pml() )
      str_series_ <- paste(str_series_, collapse = "")



      str_ <- paste0( "<c:barChart>",
              "<c:barDir val=\"%s\"/>",
              "<c:grouping val=\"%s\"/>",
              "<c:varyColors val=\"0\"/>",
              str_series_,
              "<c:dLbls><c:showLegendKey val=\"0\"/><c:showVal val=\"0\"/><c:showCatName val=\"0\"/><c:showSerName val=\"0\"/><c:showPercent val=\"0\"/><c:showBubbleSize val=\"0\"/></c:dLbls>",
              "<c:gapWidth val=\"%.0f\"/>",
              "<c:axId val=\"64451712\"/>",
              "<c:axId val=\"64453248\"/>",
              "</c:barChart>"
              )
      str_ <- sprintf(str_, private$dir, private$grouping, private$gap_width)
      str_ <- paste0("<c:plotArea ",
                     "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
                     "<c:layout/>",str_,
                     private$x_axis$pml(),
                     private$y_axis$pml(),
                     "</c:plotArea>")
                     # "<c:legend><c:legendPos val=\"r\"/><c:layout/><c:overlay val=\"0\"/></c:legend>",
                     # "<c:plotVisOnly val=\"1\"/><c:dispBlanksAs val=\"gap\"/><c:showDLblsOverMax val=\"0\"/>",
                     # "</c:chart>" )


      str_

    }

  ),
  private = list(
    dataset = NULL,
    dir = NULL,
    grouping = NULL,
    vary_colors = NULL,
    series = NULL,
    gap_width = NULL,
    x_axis = NULL,
    y_axis = NULL
  )

)

