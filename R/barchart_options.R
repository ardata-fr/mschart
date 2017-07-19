#' @export
#' @importFrom officer fp_text fp_border
barchart_options <- function( x, vary_colors = FALSE, gap_width = 150,
                              dir = "col", grouping = "clustered",
                              overlap = 0 ){

  if( !dir %in% st_bardir ){
    stop("dir should be one of ", paste0(shQuote(st_bardir), collapse = ", " ))
  }
  if( !grouping %in% st_bargrouping ){
    stop("grouping should be one of ", paste0(shQuote(st_bargrouping), collapse = ", " ))
  }

  out <- list(vary_colors=vary_colors, gap_width = gap_width, dir = dir, grouping = grouping, overlap = overlap )
  class(out) <- "barchart_options"
  out
}

