# axes ----
get_axis_tag <- function(x){
  if( inherits(x, "Date") )
    axis_tag <- "c:dateAx"
  else if( is.factor(x) || is.character(x) )
    axis_tag <- "c:catAx"
  else if( is.numeric(x) )
    axis_tag <- "c:valAx"
  else {
    stop("unknow type of data")
  }
  axis_tag
}


axis_content_xml <- function(x, id, cross_id, theme, is_x = TRUE, lab = NULL, rot = 0 ){

  x_title_id <- paste0("axis_title_", ifelse(is_x, "x", "y"))

  if( is.null(lab)) {
    title_ <- ""
  } else {
    title_ <- "<c:title><c:tx><c:rich><a:bodyPr rot=\"%.0f\" vert=\"horz\" anchor=\"ctr\"/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, rot * 60000 , format(theme[[x_title_id]], type = "pml" ), lab )
  }

  major_tm <- "<c:majorTickMark val=\"%s\"/>"
  major_tm <- sprintf(major_tm, x$major_tick_mark)

  minor_tm <- "<c:minorTickMark val=\"%s\"/>"
  minor_tm <- sprintf(minor_tm, x$minor_tick_mark)

  grid_major_id <- paste0("grid_major_line_", ifelse(is_x, "x", "y"))
  major_gl <- ooxml_fp_border(theme[[grid_major_id]],
                                in_tags = c("c:majorGridlines", "c:spPr"))

  grid_minor_id <- paste0("grid_minor_line_", ifelse(is_x, "x", "y"))
  minor_gl <- ooxml_fp_border(theme[[grid_minor_id]],
                              in_tags = c("c:minorGridlines", "c:spPr"))

  lim_max <- ""
  if( !is.null(x$limit_max) )
    lim_max <- sprintf("<c:max val=\"%.02f\"/>", x$limit_max )
  lim_min <- ""
  if( !is.null(x$limit_min) )
    lim_min <- sprintf("<c:min val=\"%.02f\"/>", x$limit_min )

  scaling_str <- sprintf("<c:scaling><c:orientation val=\"%s\"/>%s%s</c:scaling>", x$orientation, lim_max, lim_min )
  delete <- sprintf("<c:delete val=\"%.0f\"/>", x$delete )
  position <- sprintf("<c:axPos val=\"%s\"/>", x$axis_position )
  crosses <- sprintf("<c:crosses val=\"%s\"/>", x$crosses )

  lim_max <- ""
  if( !is.null(x$limit_max) )
    lim_max <- sprintf("<c:max val=\"%.02f\"/>", x$limit_max )
  lim_min <- ""
  if( !is.null(x$limit_min) )
    lim_min <- sprintf("<c:min val=\"%.02f\"/>", x$limit_min )
  cross_at <- ""
  if( !is.null(x$position) ){
    cross_at <- sprintf("<c:crossesAt val=\"%.02f\"/>", x$position )
    crosses <- ""
  }

  num_fmt <- ""
  if( !is.null(x$num_fmt) ){
    num_fmt <- sprintf("<c:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>", x$num_fmt)
  }


  tl_pos <- ""
  tl_pos <- "<c:tickLblPos val=\"%s\"/>"
  tl_pos <- sprintf(tl_pos, x$tick_label_pos)

  axis_major_ticks_id <- paste0("axis_ticks_", ifelse(is_x, "x", "y"))
  axis_ticks <- ooxml_fp_border(theme[[axis_major_ticks_id]],
                              in_tags = c("c:spPr"))


  labels_text_id <- paste0("axis_text_", ifelse(is_x, "x", "y"))
  rpr <- format(theme[[labels_text_id]], type = "pml")
  rpr <- gsub("a:rPr", "a:defRPr", rpr)
  labels_text_pr <- "<c:txPr><a:bodyPr rot=\"%.0f\" vert=\"horz\"/><a:lstStyle/><a:p><a:pPr>%s</a:pPr></a:p></c:txPr>"
  labels_text_pr <- sprintf(labels_text_pr, x$rotation * 60000, rpr )

  str_ <- paste0( "<c:axId val=\"%s\"/>",
                  scaling_str, delete, position,
                  major_gl, minor_gl,
                  title_,
                  major_tm, minor_tm, tl_pos,
                  labels_text_pr,
                  axis_ticks, num_fmt,
                  "<c:crossAx val=\"%s\"/>",
                  cross_at,
                  crosses)
  str_ <- sprintf(str_, id, cross_id)
  str_

}


axes_xml <- function(x, id_x, id_y){

  x_axis_str <- axis_content_xml( x$x_axis, id = id_x, theme = x$theme,
                                  cross_id = id_y, is_x = TRUE,
                                  lab = htmlEscape(x$labels$x), rot = x$theme$title_x_rot )
  x_axis_str <- sprintf("<%s>%s</%s>", x$axis_tag$x, x_axis_str, x$axis_tag$x)

  y_axis_str <- axis_content_xml( x$y_axis, id = id_y, theme = x$theme,
                                  cross_id = id_x, is_x = FALSE,
                                  lab = htmlEscape(x$labels$y), rot = x$theme$title_y_rot )
  y_axis_str <- sprintf("<%s>%s</%s>", x$axis_tag$y, y_axis_str, x$axis_tag$y)

  paste0(x_axis_str, y_axis_str)
}


