#' @importFrom cellranger cell_limits as.cell_limits as.range ra_ref to_string
series_wb_name <- function(dataset, idx) {
  dims <- attr(dataset, "dims")
  sheetname <- attr(dataset, "sheet")

  dims <- dims[1, idx]

  serie_range <- as.cell_limits(paste0(sheetname, "!", dims))
  serie_range
}

series_wb_data <- function(dataset, idx) {
  dims <- attr(dataset, "dims")
  sheetname <- attr(dataset, "sheet")

  dims <- dims[, idx]
  dims <- paste0(dims[2], ":", dims[length(dims)])

  serie_range <- as.cell_limits(paste0(sheetname, "!", dims))
  serie_range
}

as_series <- function(x, x_class, y_class, sheetname = "sheet1") {
  dataset <- x$data_series

  w_x <- which(names(dataset) %in% x$xvar)

  x_serie_range <- cell_limits(
    ul = c(2, w_x),
    lr = c(nrow(dataset) + 1, w_x),
    sheet = sheetname
  )

  if (inherits(dataset, "wb_data")) {
    x_serie_range <- series_wb_data(dataset, w_x)
  }

  x_serie_range <- as.range(x_serie_range, fo = "A1", strict = TRUE, sheet = TRUE)
  x_serie <- update(x_class, region = x_serie_range, values = dataset[[x$x]])

  label_columns <- get_label_names(x)

  if (inherits(dataset, "wb_data")) {
    label_columns <- x$label_cols
  }

  series <- list()

  series_nams <- get_series_names(x)
  if (x$asis) series_nams <- x$yvar

  w_y_values <- which(names(dataset) %in% series_nams)
  w_l_values <- which(names(dataset) %in% label_columns)

  has_groups <- !is.null(x$group)

  for (w_y_index in seq_along(w_y_values)) {
    w_y <- w_y_values[w_y_index]
    w_l <- w_l_values[w_y_index]
    y_colname <- names(dataset)[w_y]
    l_colname <- names(dataset)[w_l]

    serie_name_range <- ra_ref(row_ref = 1, col_ref = w_y, sheet = sheetname)
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    if (inherits(dataset, "wb_data")) {
      serie_name_range <- series_wb_name(dataset, w_y)
      serie_name_range <- as.range(serie_name_range, fo = "A1", strict = TRUE, sheet = TRUE)
    }
    serie_name <- str_ref(values = y_colname, region = serie_name_range)

    y_serie_range <- col_to_values_region(dataset, sheetname, y_colname)
    y_serie <- update(y_class, region = y_serie_range, values = dataset[[y_colname]])

    label_serie <- NULL
    if (length(label_columns) > 0) {
      label_serie_range <- col_to_values_region(dataset, sheetname, l_colname)
      label_serie <- label_ref(values = dataset[[l_colname]], region = label_serie_range)
    }

    error_bars <- list(y = .create_empty_error_bars())

    if (!is.null(x$error_bar_colnames$y$lower)) {
      y_lower_colname <- .error_bar_colname(x$error_bar_colnames$y$lower, "y", "lower")
      if (has_groups) y_lower_colname <- paste(y_lower_colname, y_colname, sep = "_")

      y_lower_range <- col_to_values_region(dataset, sheetname, y_lower_colname)
      error_bars$y$ref_lower <- num_ref(values = dataset[[y_lower_colname]], region = y_lower_range)
    }

    if (!is.null(x$error_bar_colnames$y$upper)) {
      y_upper_colname <- .error_bar_colname(x$error_bar_colnames$y$upper, "y", "upper")
      if (has_groups) y_upper_colname <- paste(y_upper_colname, y_colname, sep = "_")

      y_upper_range <- col_to_values_region(dataset, sheetname, y_upper_colname)
      error_bars$y$ref_upper <- num_ref(values = dataset[[y_upper_colname]], region = y_upper_range)
    }

    ser <- list(
      idx = length(series), order = length(series),
      tx = serie_name,
      x = x_serie, y = y_serie, label = label_serie,
      error_bars = error_bars,
      stroke = x$series_settings$colour[y_colname],
      fill = x$series_settings$fill[y_colname],
      symbol = x$series_settings$symbol[y_colname],
      line_style = x$series_settings$line_style[y_colname],
      size = x$series_settings$size[y_colname],
      line_width = x$series_settings$line_width[y_colname],
      labels_fp = x$series_settings$labels_fp[[y_colname]],
      smooth = x$series_settings$smooth[y_colname]
    )
    series <- append(series, list(ser))
  }
  series
}

#' @importFrom cellranger cell_limits as.range
col_to_values_region <- function(dataset, sheetname, colname) {
  col_index <- which(names(dataset) == colname)

  cells <- if (inherits(dataset, "wb_data")) {
    series_wb_data(dataset, col_index)
  } else {
    cell_limits(ul = c(2, col_index), lr = c(nrow(dataset) + 1, col_index), sheet = sheetname)
  }

  as.range(cells, fo = "A1", strict = TRUE, sheet = TRUE)
}
