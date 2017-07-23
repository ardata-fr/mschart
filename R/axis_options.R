#' @export
axis_options <- function( orientation = "minMax", axis_position = "b",
                          crosses = "autoZero", cross_between = "between",
                          major_tick_mark = "cross", minor_tick_mark = "none",
                          tick_label_pos = "nextTo", delete = FALSE, num_fmt = NULL,
                          rotation = 0 ){

  if( !orientation %in% st_orientation ){
    stop("orientation should be one of ", paste0(shQuote(st_orientation), collapse = ", " ))
  }
  if( !axis_position %in% st_axpos ){
    stop("axis_position should be one of ", paste0(shQuote(st_axpos), collapse = ", " ))
  }
  if( !crosses %in% st_crosses ){
    stop("crosses should be one of ", paste0(shQuote(st_crosses), collapse = ", " ))
  }
  if( !cross_between %in% st_crossbetween ){
    stop("cross_between should be one of ", paste0(shQuote(st_crossbetween), collapse = ", " ))
  }
  if( !major_tick_mark %in% st_tickmark ){
    stop("major_tick_mark should be one of ", paste0(shQuote(st_tickmark), collapse = ", " ))
  }
  if( !minor_tick_mark %in% st_tickmark ){
    stop("minor_tick_mark should be one of ", paste0(shQuote(st_tickmark), collapse = ", " ))
  }
  if( !tick_label_pos %in% st_ticklblpos ){
    stop("tick_label_pos should be one of ", paste0(shQuote(st_ticklblpos), collapse = ", " ))
  }

  out <- list(
    orientation = orientation,
    axis_position = axis_position,
    crosses = crosses,
    delete = delete,
    num_fmt = num_fmt,
    major_tick_mark = major_tick_mark,
    minor_tick_mark = minor_tick_mark,
    tick_label_pos = tick_label_pos,
    rotation = rotation
  )
  class(out) <- "axis_options"
  out

}


#' @export
pml_axis_options <- function(x, id, cross_id, theme, is_x = TRUE, lab = NULL, vertical_align = "horz", rot = 0 ){

  x_title_id <- paste0("axis.title.", ifelse(is_x, "x", "y"))

  if( is.null(lab)) {
    title_ <- ""
  } else {
    title_ <- "<c:title><c:tx><c:rich><a:bodyPr rot=\"%.0f\" vert=\"%s\" anchor=\"ctr\"/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, rot * 60000 , vertical_align, format(theme[[x_title_id]], type = "pml" ), lab )
  }

  grid_major_id <- paste0("grid.major.line.", ifelse(is_x, "x", "y"))
  major_gl <- "<c:majorGridlines><c:spPr>%s</c:spPr></c:majorGridlines>"
  major_gl <- sprintf(major_gl, format(theme[[grid_major_id]], type = "pml") )

  grid_minor_id <- paste0("grid.minor.line.", ifelse(is_x, "x", "y"))
  minor_gl <- "<c:minorGridlines><c:spPr>%s</c:spPr></c:minorGridlines>"
  minor_gl <- sprintf(minor_gl, format(theme[[grid_minor_id]], type = "pml") )

  orientation <- sprintf("<c:scaling><c:orientation val=\"%s\"/></c:scaling>", x$orientation )
  delete <- sprintf("<c:delete val=\"%.0f\"/>", x$delete )
  position <- sprintf("<c:axPos val=\"%s\"/>", x$axis_position )
  crosses <- sprintf("<c:crosses val=\"%s\"/>", x$crosses )
  num_fmt <- sprintf("<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>", x$num_fmt)

  major_tm <- ""
  major_tm <- "<c:majorTickMark val=\"%s\"/>"
  major_tm <- sprintf(major_tm, x$major_tick_mark)

  minor_tm <- ""
  minor_tm <- "<c:minorTickMark val=\"%s\"/>"
  minor_tm <- sprintf(minor_tm, x$minor_tick_mark)

  tl_pos <- ""
  tl_pos <- "<c:tickLblPos val=\"%s\"/>"
  tl_pos <- sprintf(tl_pos, x$tick_label_pos)

  axis_major_ticks_id <- paste0("axis.ticks.", ifelse(is_x, "x", "y"))
  axis_ticks <- "<c:spPr>%s</c:spPr>"
  axis_ticks <- sprintf(axis_ticks, format(theme[[axis_major_ticks_id]], type = "pml") )


  labels_text_id <- paste0("axis.text.", ifelse(is_x, "x", "y"))
  rpr <- format(theme[[labels_text_id]], type = "pml")
  rpr <- gsub("a:rPr", "a:defRPr", rpr)
  labels_text_pr <- "<c:txPr><a:bodyPr rot=\"%.0f\" vert=\"horz\"/><a:lstStyle/><a:p><a:pPr>%s</a:pPr></a:p></c:txPr>"
  labels_text_pr <- sprintf(labels_text_pr, x$rotation * 60000, rpr )

  str_ <- paste0( "<c:axId val=\"%s\"/>",
                  orientation, delete, position,
                  major_gl, minor_gl,
                  title_,
                  major_tm, minor_tm, tl_pos,
                  labels_text_pr,
                  axis_ticks, num_fmt,
                  "<c:crossAx val=\"%s\"/>", crosses)
  str_ <- sprintf(str_, id, cross_id)
  str_

}
