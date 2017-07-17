# axe_title ----
#' @importFrom R6 R6Class
axe_title <- R6::R6Class(
  "axe_title",
  public = list(
    initialize = function( par, vertical_align ) {

      private$vertical_align <- vertical_align
      private$par <- par

    },
    pml = function(){
      str_ <- "<c:title><c:tx><c:rich><a:bodyPr vert=\"%s\" anchor==\"ctr\"/><a:lstStyle/>%s</c:rich></c:tx></c:title>"
      sprintf(str_, private$vertical_align, format(private$par, type = "pml"))
    }
  ),
  private = list(
    par = NULL,
    vertical_align = NULL
  )
)


# sp_pr ----
#' @export
sp_pr <- R6::R6Class(
  "sp_pr",
  public = list(
    initialize = function( type = "solid", width = 1, colour = "black" ) {
      private$type <- type
      private$width <- width
      col_mat <- col2rgb(colour, alpha = TRUE)
      private$red <- col_mat[1,1]
      private$green <- col_mat[2,1]
      private$blue <- col_mat[3,1]
      private$alpha <- col_mat[4,1]
    },
    pml = function(){

      str_1 <- sprintf("<a:ln w=\"%.0f\">", private$width * 12700 )
      str_2 <- sprintf("<a:solidFill><a:srgbClr val=\"%02X%02X%02X\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>",
                       private$red, private$green, private$blue, private$alpha/255 * 100000)
      str_3 <- sprintf("<a:prstDash val=\"%s\"/>", private$type)
      paste0("<c:spPr>", str_1, str_2, str_3, "</a:ln></c:spPr>")

    }
  ),
  private = list(
    type = NULL,
    width = NULL,
    red = NULL,
    green = NULL,
    blue = NULL,
    alpha = NULL
  )
)

# base_axis ----
#' @export
base_axis <- R6::R6Class(
  "base_axis",
  public = list(
    initialize = function( id, orientation, axis_position, cross_ax,
                           major_tick_mark = "cross", minor_tick_mark = "none",
                           tick_label_pos = "nextTo",
                           delete = FALSE
                           ) {

      stopifnot( orientation %in% c('maxMin', 'minMax') )
      stopifnot( major_tick_mark %in% c('cross', 'in', 'none', 'out') )
      stopifnot( minor_tick_mark %in% c('cross', 'in', 'none', 'out') )
      stopifnot( tick_label_pos %in% c('high', 'low', 'nextTo', 'none') )

      private$id <- id
      private$orientation <- orientation
      private$axis_position <- axis_position
      private$delete <- delete
      private$cross_ax <- cross_ax
      private$major_tick_mark <- major_tick_mark
      private$minor_tick_mark <- minor_tick_mark
      private$tick_label_pos <- tick_label_pos
    },

    set_delete = function(value = TRUE){
      private$delete <- value
      self
    },
    set_major_grid_lines = function(value = sp_pr$new()){
      private$major_grid_lines <- value
      self
    },
    set_minor_grid_lines = function(value = sp_pr$new()){
      private$minor_grid_lines <- value
      self
    },
    set_title = function(value = NULL){
      private$title <- value
      self
    },
    set_num_format = function(value = "#,##0.00"){
      private$num_fmt <- value
      self
    },
    base_pml = function(){

      mgl <- ""
      if( !is.null(private$major_grid_lines)){
        mgl <- "<c:majorGridlines>%s</c:majorGridlines>"
        mgl <- sprintf(mgl, private$major_grid_lines$pml())
      }
      major_tm <- ""
      if( !is.null(private$major_tick_mark)){
        major_tm <- "<c:majorTickMark val=\"%s\"/>"
        major_tm <- sprintf(major_tm, private$major_tick_mark)
      }
      minor_tm <- ""
      if( !is.null(private$minor_tick_mark)){
        minor_tm <- "<c:minorTickMark val=\"%s\"/>"
        minor_tm <- sprintf(minor_tm, private$minor_tick_mark)
      }
      tl_pos <- ""
      if( !is.null(private$tick_label_pos)){
        tl_pos <- "<c:tickLblPos val=\"%s\"/>"
        tl_pos <- sprintf(tl_pos, private$tick_label_pos)
      }


      str_ <- paste0( "<c:axId val=\"%s\"/>",
                      "<c:scaling><c:orientation val=\"%s\"/></c:scaling>",
                      ifelse( private$delete, "<c:delete val=\"1\"/>", "<c:delete val=\"0\"/>"),
                      "<c:axPos val=\"%s\"/>",
                      mgl,
                      ifelse( is.null(private$minor_grid_lines), "", private$minor_grid_lines$pml() ),
                      ifelse( is.null(private$title), "", private$title$pml() ),
                      major_tm, minor_tm, tl_pos,
                      ifelse( is.null(private$num_fmt), "", sprintf("<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>", private$num_fmt) ),
                      "<c:crossAx val=\"%s\"/>",
                      "<c:crosses val=\"autoZero\"/>")
      str_ <- sprintf(str_, private$id, private$orientation,
                      private$axis_position, private$cross_ax)
      str_

    }

  ),
  private = list(
    # mandatory
    id = NULL,
    orientation = NULL,
    axis_position = NULL,
    cross_ax = NULL,
    # optional
    delete = NULL,
    major_grid_lines = NULL,
    minor_grid_lines = NULL,
    title = NULL,
    num_fmt = NULL,
    major_tick_mark = NULL,
    minor_tick_mark = NULL,
    tick_label_pos = NULL,
    sp_pr = NULL,
    tx_pr = NULL
  )
)



#' @export
# cat_axis ----
cat_axis <- R6::R6Class(
  "cat_axis",
  inherit =  base_axis,
  public = list(
    initialize = function( id, orientation, axis_position, cross_ax){
      super$initialize(id = id, orientation = orientation,
                       axis_position = axis_position, cross_ax = cross_ax)

    },

    pml = function(){
      private$major_grid_lines <- NULL
      private$minor_grid_lines <- NULL

      str_ <- "<c:catAx>%s</c:catAx>"
      sprintf(str_, self$base_pml())
    }
  )

)



#' @export
# val_axis ----
val_axis <- R6::R6Class(
  "val_axis",
  inherit =  base_axis,
  public = list(
    initialize = function( id, orientation, axis_position, cross_ax){
      super$initialize(id = id, orientation = orientation,
                       axis_position = axis_position, cross_ax = cross_ax)
      self$set_num_format(value = "#,##0.00")
      self
    },

    pml = function(){
      str_ <- "<c:valAx>%s</c:valAx>"
      sprintf(str_, self$base_pml())
    }
  )

)



