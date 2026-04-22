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

as_series <- function(
  x,
  x_class,
  y_class,
  sheetname = "sheet1",
  secondary_y = 0
) {
  dataset <- x$data_series

  # Optional offset when the data is written to a non-default cell
  # position in the host sheet (see sheet_add_drawing.ms_chart).
  data_start_col <- x$data_start_col %||% 1L
  data_start_row <- x$data_start_row %||% 1L
  col_offset <- data_start_col - 1L
  header_row <- data_start_row
  first_val_row <- data_start_row + 1L
  last_val_row <- data_start_row + nrow(dataset)

  w_x <- which(names(dataset) %in% x$xvar)

  x_serie_range <- cell_limits(
    ul = c(first_val_row, w_x + col_offset),
    lr = c(last_val_row,  w_x + col_offset),
    sheet = sheetname
  )

  if (inherits(dataset, "wb_data")) {
    x_serie_range <- series_wb_data(dataset, w_x)
  }

  x_serie_range <- as.range(
    x_serie_range,
    fo = "A1",
    strict = TRUE,
    sheet = TRUE
  )
  x_serie <- update(x_class, region = x_serie_range, values = dataset[[x$x]])

  label_columns <- get_label_names(x)

  if (inherits(dataset, "wb_data")) {
    label_columns <- x$label_cols
  }

  series <- list()

  series_nams <- get_series_names(x)
  if (x$asis) {
    series_nams <- x$yvar
  }

  w_y_values <- which(names(dataset) %in% series_nams)
  w_l_values <- which(names(dataset) %in% label_columns)

  for (w_y_index in seq_along(w_y_values)) {
    w_y <- w_y_values[w_y_index]
    w_l <- w_l_values[w_y_index]
    y_colname <- names(dataset)[w_y]
    l_colname <- names(dataset)[w_l]

    serie_name_range <- ra_ref(row_ref = header_row,
                               col_ref = w_y + col_offset,
                               sheet = sheetname)
    serie_name_range <- to_string(serie_name_range, fo = "A1")
    if (inherits(dataset, "wb_data")) {
      serie_name_range <- series_wb_name(dataset, w_y)
      serie_name_range <- as.range(
        serie_name_range,
        fo = "A1",
        strict = TRUE,
        sheet = TRUE
      )
    }
    serie_name <- str_ref(values = y_colname, region = serie_name_range)

    y_serie_range <- cell_limits(
      ul = c(first_val_row, w_y + col_offset),
      lr = c(last_val_row,  w_y + col_offset),
      sheet = sheetname
    )

    if (inherits(dataset, "wb_data")) {
      y_serie_range <- series_wb_data(dataset, w_y)
    }
    y_serie_range <- as.range(
      y_serie_range,
      fo = "A1",
      strict = TRUE,
      sheet = TRUE
    )

    y_serie <- update(
      y_class,
      region = y_serie_range,
      values = dataset[[y_colname]]
    )

    if (length(label_columns) > 0) {
      label_serie_range <- cell_limits(
        ul = c(first_val_row, w_l + col_offset),
        lr = c(last_val_row,  w_l + col_offset),
        sheet = sheetname
      )
      label_serie_range <- as.range(
        label_serie_range,
        fo = "A1",
        strict = TRUE,
        sheet = TRUE
      )

      if (inherits(dataset, "wb_data")) {
        label_serie_range <- series_wb_data(dataset, w_l)
        label_serie_range <- as.range(
          label_serie_range,
          fo = "A1",
          strict = TRUE,
          sheet = TRUE
        )
      }
      label_serie <- label_ref(
        values = dataset[[l_colname]],
        region = label_serie_range
      )
    } else {
      label_serie <- NULL
    }

    # bubble size series (for bubble charts)
    bubble_size_serie <- NULL
    if (!is.null(x$size_cols)) {
      sz_col <- x$size_cols[1]
      if (!is.null(x$group)) {
        # transposed name: "sz-groupname"
        grp_name <- sub("^.*-", "", y_colname)
        sz_colname <- paste0(sz_col, "-", grp_name)
      } else {
        sz_colname <- sz_col
      }
      if (sz_colname %in% names(dataset)) {
        w_sz <- which(names(dataset) == sz_colname)
        sz_range <- cell_limits(
          ul = c(first_val_row, w_sz + col_offset),
          lr = c(last_val_row,  w_sz + col_offset),
          sheet = sheetname
        )
        sz_range <- as.range(sz_range, fo = "A1", strict = TRUE, sheet = TRUE)
        bubble_size_serie <- update(
          num_ref(values = numeric(0), region = ""),
          region = sz_range,
          values = dataset[[sz_colname]]
        )
      }
    }

    ser <- list(
      idx = length(series) + secondary_y,
      order = length(series) + secondary_y,
      tx = serie_name,
      x = x_serie,
      y = y_serie,
      bubble_size = bubble_size_serie,
      label = label_serie,
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
