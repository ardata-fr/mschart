#' @title xml code for chart and plot area properties
#' @param theme an object of class mschart_theme
#' @param what a character. One of `"plot"` or `"chart"`
#' @noRd
sppr_content_xml <- function(theme = NULL, what = "chart", ns = NULL) {
  if (!what %in% c("plot", "chart")) {
    stop("what sould be one of \"plot\" or \"chart\"")
  }

  fill <- theme[[paste0(what, "_background")]]
  border_properties <- theme[[paste0(what, "_border")]]

  if (!is.null(fill)) {
    fill_elts <- col2rgb(fill, alpha = TRUE)[, 1]
    fill_hex <- sprintf("%02X%02X%02X", fill_elts[1], fill_elts[2], fill_elts[3])
    fill_str <- sprintf(
      "<a:solidFill><a:srgbClr val=\"%s\"><a:alpha val=\"%.0f\"/></a:srgbClr></a:solidFill>",
      fill_hex, fill_elts[4] / 255.0 * 100000
    )
  } else {
    fill_str <- NULL
  }

  border_str <- ooxml_fp_border(border_properties)

  if (!is.null(ns)) ns <- paste0(" ", ns)

  sppr_str <- paste0(
    "<c:spPr", ns, ">",
    fill_str,
    border_str,
    "</c:spPr>"
  )

  sppr_str
}
