#' @export
scatter_options <- function( x, vary_colors = FALSE, scatter_style = "lineMarker"){

  if( !scatter_style %in% st_scatterstyle ){
    stop("scatter_style should be one of ", paste0(shQuote(st_scatterstyle), collapse = ", " ))
  }

  out <- list(vary_colors = vary_colors, scatter_style = scatter_style )
  class(out) <- "scatter_options"
  out
}

