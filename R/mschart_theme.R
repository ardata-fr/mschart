#' @export
#' @importFrom officer fp_text fp_border
mschart_theme <- function(axis.title = fp_text(bold = TRUE, font.size = 16), axis.title.x = axis.title, axis.title.y = axis.title,
                     axis.text = fp_text(), axis.text.x = axis.text, axis.text.y = axis.text,
                     axis.ticks = fp_border(color = "#99999999"), axis.ticks.x = axis.ticks, axis.ticks.y = axis.ticks,
                     grid.major.line = fp_border(color = "#99999999", style = "dashed"), grid.major.line.x = grid.major.line, grid.major.line.y = grid.major.line,
                     grid.minor.line = fp_border(width = 0), grid.minor.line.x = grid.minor.line, grid.minor.line.y = grid.minor.line,
                     plot.title.text = fp_text(bold = TRUE, font.size = 20) ){

  stopifnot(inherits(plot.title.text, "fp_text"))
  stopifnot(inherits(axis.title, "fp_text"))
  stopifnot(inherits(axis.title.x, "fp_text"))
  stopifnot(inherits(axis.title.y, "fp_text"))
  stopifnot(inherits(axis.text, "fp_text"))
  stopifnot(inherits(axis.text.x, "fp_text"))
  stopifnot(inherits(axis.text.y, "fp_text"))
  stopifnot(inherits(axis.ticks, "fp_border"))
  stopifnot(inherits(axis.ticks.x, "fp_border"))
  stopifnot(inherits(axis.ticks.y, "fp_border"))
  stopifnot(inherits(grid.major.line, "fp_border"))
  stopifnot(inherits(grid.major.line.x, "fp_border"))
  stopifnot(inherits(grid.major.line.y, "fp_border"))
  stopifnot(inherits(grid.minor.line, "fp_border"))
  stopifnot(inherits(grid.minor.line.x, "fp_border"))
  stopifnot(inherits(grid.minor.line.y, "fp_border"))


  out <- list(axis.title = axis.title, axis.title.x = axis.title.x, axis.title.y = axis.title.y,
       axis.text = axis.text, axis.text.x = axis.text.x, axis.text.y = axis.text.y,
       axis.ticks = axis.ticks, axis.ticks.x = axis.ticks.x, axis.ticks.y = axis.ticks.y,
       grid.major.line = grid.major.line, grid.major.line.x = grid.major.line.x, grid.major.line.y = grid.major.line.y,
       grid.minor.line = grid.minor.line, grid.minor.line.x = grid.minor.line.x, grid.minor.line.y = grid.minor.line.y,
       plot.title.text = plot.title.text)
  class(out) <- "mschart_theme"
  out
}

