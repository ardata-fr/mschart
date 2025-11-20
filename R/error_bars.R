.create_empty_error_bars <- function() {
  structure(
    list(
      ref_lower = NULL,
      ref_upper = NULL
    ),
    class = "ms_error_bars"
  )
}

.error_bar_colnames <- function(x) {
  stopifnot(inherits(x, "ms_chart"))

  out <- c()

  y_lower <- x$error_bar_colnames$y$lower
  if (!is.null(y_lower)) {
    out <- c(out, y_lower, .error_bar_colname(y_lower, "y", "lower"))
  }

  y_upper <- x$error_bar_colnames$y$upper
  if (!is.null(y_upper)) {
    out <- c(out, y_upper, .error_bar_colname(y_upper, "y", "upper"))
  }

  out
}

.error_bar_colname <- function(name, axis = c("x", "y"), sign = c("lower", "upper")) {
  axis <- match.arg(axis)
  sign <- match.arg(sign)

  sprintf("%s (%s-axis %s error value)", name, toupper(axis), sign)
}

#' @export
#' @method to_pml ms_error_bars
to_pml.ms_error_bars <- function(x, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (is.null(x$ref_lower) && is.null(x$ref_upper)) {
    return("")
  }

  err_bar_type <- if (!is.null(x$ref_lower) && !is.null(x$ref_upper)) {
    "both"
  } else if (!is.null(x$ref_lower)) {
    "minus"
  } else {
    "plus"
  }

  paste0(
    "<c:errBars>",
    sprintf('<c:errDir val="%s"/>', axis),
    sprintf('<c:errBarType val="%s"/>', err_bar_type),
    '<c:errValType val="cust"/>',
    '<c:noEndCap val="0"/>',
    if (!is.null(x$ref_lower)) paste0("<c:minus>", to_pml(x$ref_lower), "</c:minus>"),
    if (!is.null(x$ref_upper)) paste0("<c:plus>", to_pml(x$ref_upper), "</c:plus>"),
    "</c:errBars>"
  )
}
