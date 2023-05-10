## -- assert functions

assert_area <- function(data_x, data_y) {
  if (!is.numeric(data_y)) {
    stop("y column should be numeric.")
  }
  check_x <- inherits(data_x, "Date") || is.character(data_x) || is.factor(data_x)
  if (!check_x) {
    stop("x column should be a date or a categorical column.")
  }
}

assert_scatter <- function(data_x, data_y) {
  if (!is.numeric(data_y)) {
    stop("y column should be numeric.")
  }
  if (!is.numeric(data_x)) {
    stop("x column should be numeric.")
  }
}

assert_line <- function(data_y) {
  if (!is.numeric(data_y)) {
    stop("y column should be numeric.")
  }
}

assert_pie <- function(data_x, data_y) {
  if (!is.numeric(data_y)) {
    stop("y column should be numeric.")
  }
  if (is.numeric(data_x)) {
    stop("x column should be a categorical column (character or factor).")
  }
}

#' @title Linechart object
#' @description Creation of a linechart object that can be
#' inserted in a 'Microsoft' document.
#'
#' In a line chart, category data is distributed evenly along the horizontal axis, and
#' all value data is distributed evenly along the vertical axis. Line charts can show
#' continuous data over time on an evenly scaled axis, so they're ideal for showing
#' trends in data at equal intervals, like months and quarters.
#' @param data a data.frame
#' @param x column name for x values.
#' @param y column name for y values.
#' @param group grouping column name used to split data into series. Optional.
#' @param labels column names of columns to be used as custom data labels
#' displayed next to data points (not axis labels). Optional.
#' If more than one name is provided, only the first one will be used as a label, but all
#' labels (transposed if a group is used) will be available in the Excel file
#' associated with the chart.
#' @param asis logical parameter defaulting to FALSE. When FALSE, the data is
#' reshaped internally so that each series becomes a separate column. When TRUE,
#' the data is used as-is and must already have one column for categories and
#' one column per series.
#' @return An `ms_chart` object.
#' @export
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_ax_x()], [chart_ax_y()],
#' [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_ms_linechart_1.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_linechart_2.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_linechart_3.png}{options: width="500"}}
#' @examples
#' library(officer)
#' @example examples/02_linechart.R
ms_linechart <- function(data, x, y, group = NULL, labels = NULL, asis = FALSE) {
  out <- ms_chart(
    data = data, x = x, y = y, group = group, labels = labels,
    type = "lineplot", asis = asis
  )
  out$options <- linechart_options()
  class(out) <- c("ms_linechart", "ms_chart")
  out <- chart_settings(out)
  out
}

#' @title Barchart object
#' @description Creation of a barchart object that can be
#' inserted in a 'Microsoft' document.
#'
#' Bar charts illustrate comparisons among individual items. In a bar chart, the
#' categories are typically organized along the vertical axis, and the values
#' along the horizontal axis.
#'
#' Consider using a bar chart when:
#'
#' * The axis labels are long.
#' * The values that are shown are durations.
#' @inheritParams ms_linechart
#' @return An `ms_chart` object.
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_ax_x()], [chart_ax_y()],
#' [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @export
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_ms_barchart_1.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_barchart_2.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_barchart_3.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_barchart_4.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_barchart_5.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_barchart_6.png}{options: width="500"}}
#' @examples
#' library(officer)
#' @example examples/01_barchart.R
ms_barchart <- function(data, x, y, group = NULL, labels = NULL, asis = FALSE) {
  out <- ms_chart(
    data = data, x = x, y = y, group = group, labels = labels,
    type = "barplot", asis = asis
  )
  out$options <- barchart_options()
  class(out) <- c("ms_barchart", "ms_chart")
  out
}

#' @title Areachart object
#' @description Creation of an areachart object that can be
#' inserted in a 'Microsoft' document.
#'
#' Area charts can be used to plot change over time and draw attention to the
#' total value across a trend. By showing the sum of the plotted values, an area
#' chart also shows the relationship of parts to a whole.
#' @inheritParams ms_linechart
#' @return An `ms_chart` object.
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_ax_x()], [chart_ax_y()],
#' [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @export
#' @examples
#' library(officer)
#' @example examples/03_areachart.R
ms_areachart <- function(data, x, y, group = NULL, labels = NULL, asis = FALSE) {
  out <- ms_chart(
    data = data, x = x, y = y, group = group, labels = labels,
    type = "areaplot", asis = asis
  )
  class(out) <- c("ms_areachart", "ms_chart")
  out <- chart_settings(out)

  serie_names <- names(out$series_settings$colour)
  values <- setNames(rep("transparent", length(serie_names)), serie_names)
  out <- chart_data_stroke(out, values = values)

  out
}

#' @title Scatterchart object
#' @description Creation of a scatterchart object that can be
#' inserted in a 'Microsoft' document.
#' @inheritParams ms_linechart
#' @return An `ms_chart` object.
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_ax_x()], [chart_ax_y()],
#' [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @export
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_ms_scatterchart_1.png}{options: width="500"}}
#'
#' \if{html}{\figure{fig_ms_scatterchart_2.png}{options: width="500"}}
#' @examples
#' library(officer)
#' @example examples/04_scatterchart.R
ms_scatterchart <- function(data, x, y, group = NULL, labels = NULL, asis = FALSE) {
  out <- ms_chart(
    data = data, x = x, y = y, group = group, labels = labels,
    excel_data_setup = transpose_series_bysplit,
    type = "scatterplot", asis = asis
  )
  class(out) <- c("ms_scatterchart", "ms_chart")

  out <- chart_settings(out)

  out
}

#' @title combochart object
#' @description Creation of a combochart object that can be
#' inserted in a 'Microsoft' document.
#' @details ms_combochart only works with mschart objects created with
#' `asis = TRUE`.
#' @param ... mschart objects
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_ax_x()], [chart_ax_y()],
#' [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @export
ms_combochart <- function(...) {

  inputs <- list(...)

  out <-  inputs[[1]]

  sec_cntr_x <- 0
  sec_cntr_y <- 0

  for (i in seq_along(inputs)[-1]) {

    if (!inherits(inputs[[i]], "ms_chart")) {
      warning("skipping element: ", inputs[[i]])
      next
    }

    is_sec_x <- isTRUE(attr(inputs[[i]], "secondary_x"))
    is_sec_y <- isTRUE(attr(inputs[[i]], "secondary_y"))

    sec_cntr_x <- sum(sec_cntr_x, is_sec_x)
    sec_cntr_y <- sum(sec_cntr_y, is_sec_y)

    # avoid additional labels. only one axis label and one title per chart
    # title and x axis have to be defined in the first mschart
    lbl <- inputs[[i]]$labels
    xlab <- lbl$x
    ylab <- lbl$y

    # disable additional x and y axis for add and secondary_y
    if ((i > 1 && !is_sec_y&& !is_sec_x) || (sec_cntr_y > 1 && is_sec_y)) {
      inputs[[i]]$x_axis <- axis_options(axis_position = "b", delete = 1L)
      inputs[[i]]$y_axis <- axis_options(axis_position = "l", delete = 1L)
      ylab <- NULL
    } else if ((i > 1 && !is_sec_y && !is_sec_x) || (sec_cntr_x > 1 && is_sec_x)) {
      inputs[[i]]$x_axis <- axis_options(axis_position = "t", delete = 1L)
      inputs[[i]]$y_axis <- axis_options(axis_position = "l", delete = 1L)
      xlab <- NULL
    }

    inputs[[i]]$labels$title <- list(title = NULL, x = xlab, y = ylab)

    if (is.null(out$secondary)) {
      out$secondary <- list(inputs[[i]])
    } else {
      out$secondary <- append(out$secondary, list(inputs[[i]]))
    }
  }

  out
}

#' @title Piechart object
#' @description Creation of a piechart object that can be
#' inserted in a 'Microsoft' document.
#'
#' Pie charts show the proportion of each category as a slice
#' of a circle. Doughnut charts are similar but have a hole
#' in the centre. Use `chart_settings(x, hole_size = ...)` to
#' control the hole size: 0 produces a pie chart, values
#' above 0 produce a doughnut chart.
#'
#' Data must be pre-aggregated: one row per slice, no grouping
#' column.
#' @param data a data.frame
#' @param x column name for categories (slices).
#' @param y column name for values (slice sizes).
#' @param labels column names of columns to be used as custom data labels
#' displayed next to data points (not axis labels). Optional.
#' If more than one name is provided, only the first one will be used as a label, but all
#' labels (transposed if a group is used) will be available in the Excel file
#' associated with the chart.
#' @return An `ms_chart` object.
#' @export
#' @family 'Office' chart objects
#' @seealso [chart_settings()], [chart_data_labels()], [chart_theme()], [chart_labels()]
#' @examples
#' library(officer)
#' library(mschart)
#'
#' dat <- data.frame(
#'   browser = c("Chrome", "Firefox", "Safari", "Edge", "Other"),
#'   value = c(64, 12, 8, 5, 11)
#' )
#'
#' # Pie chart
#' pie <- ms_piechart(data = dat, x = "browser", y = "value")
#' pie <- chart_labels(pie, title = "Browser share")
#'
#' # Doughnut chart
#' donut <- ms_piechart(data = dat, x = "browser", y = "value")
#' donut <- chart_settings(donut, hole_size = 50)
#' donut <- chart_labels(donut, title = "Browser share (donut)")
ms_piechart <- function(data, x, y, labels = NULL) {
  out <- ms_chart(
    data = data, x = x, y = y, group = NULL, labels = labels,
    type = "pieplot"
  )
  out$options <- piechart_options()
  class(out) <- c("ms_piechart", "ms_chart")
  out <- chart_settings(out)

  serie_names <- names(out$series_settings$colour)
  values <- setNames(rep("transparent", length(serie_names)), serie_names)
  out <- chart_data_stroke(out, values = values)

  out
}


# ms_chart -----

#' @importFrom grDevices colors
ms_chart <- function(data, x, y, group = NULL, labels = NULL,
                     excel_data_setup = shape_as_series,
                     type = NULL, asis = FALSE) {
  stopifnot(is.data.frame(data))
  stopifnot(x %in% names(data))
  stopifnot(y %in% names(data))

  # if wb_data is passed, only create asis mschart output
  if (inherits(data, "wb_data")) asis <- TRUE

  xvar <- x
  yvar <- y

  if (inherits(data, "data.table") || inherits(data, "tbl_df") || inherits(data, "tbl")) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }

  if (!is.null(group) && !(group %in% names(data))) {
    stop("column ", shQuote(group), " could not be found in data.", call. = FALSE)
  }
  if (!is.null(labels)) {
    labs <- labels[!labels %in% names(data)]
    if (!(all(labs))) {
      stop("column(s) ", paste(shQuote(labs), collapse = ", "), " could not be found in data.", call. = FALSE)
    }
  }

  theme_ <- mschart_theme()

  if (asis) {
    data_x <- sort(unname(unlist(data[[x]])))
  } else {
    data_x <- data[[x]]
  }

  if (asis) {
    data_y <- sort(unname(unlist(data[y])))
  } else {
    data_y <- data[[y]]
  }

  if (type == "areaplot" || type == "barplot") {
    assert_area(data_x, data_y)
  }

  if (type == "scatterplot") {
    assert_scatter(data_x, data_y)
  }

  if (type == "lineplot") {
    assert_line(data_y)
  }

  if (type == "pieplot") {
    assert_pie(data_x, data_y)
  }


  tryCatch(
    {
      x_axis_tag <- get_axis_tag(data_x)
    },
    error = function(e) {
      stop("column ", shQuote(x), ": ", e$message, " [", paste(class(data_x), collapse = ","), "]", call. = FALSE)
    }
  )
  tryCatch(
    {
      y_axis_tag <- get_axis_tag(data_y)
    },
    error = function(e) {
      stop("column ", shQuote(y), ": ", e$message, " [", paste(class(data_y), collapse = ","), "]", call. = FALSE)
    }
  )

  x_axis_ <- axis_options(axis_position = "b")
  y_axis_ <- axis_options(axis_position = "l")

  x <- x[1]
  y <- y[1]

  lbls <- list(title = NULL, x = x, y = y)

  out <- list(
    data = data,
    x = x,
    y = y,
    group = group,
    label_cols = labels,
    theme = theme_,
    options = list(),
    x_axis = x_axis_,
    y_axis = y_axis_,
    axis_tag = list(
      x = x_axis_tag,
      y = y_axis_tag
    ),
    fmt_names = list(
      x = fmt_name(data_x),
      y = fmt_name(data_y)
    ),
    labels = lbls,
    asis = asis,
    xvar = xvar,
    yvar = yvar
  )
  class(out) <- c("ms_chart")
  out <- chart_data_labels(out)

  if (type == "areaplot" || type == "lineplot") {
    xtag <- if (inherits(data_x, "Date")) {
      "c:dateAx"
    } else if (is.character(data_x) || is.factor(data_x)) {
      "c:catAx"
    } else {
      "c:valAx"
    }
    out$axis_tag <- list(x = xtag, y = "c:valAx")
  }

  if (type == "scatterplot") {
    out <- pretty_num_axes(out, data_x, data_y)
  }

  if (!asis) {
    out$data_series <- excel_data_setup(out)
    series_names <- get_series_names(out)
  } else {
    out$data_series <- out$data
    series_names <- names(out$data)[-1]
  }

  if (length(series_names) <= length(colour_list)) {
    palette_ <- colour_list[[length(series_names)]]
  } else {
    palette_ <- sample(colors(), size = length(series_names), replace = TRUE)
  }

  series_symbols <- rep("circle", length(series_names))
  series_lstyle <- rep("solid", length(series_names))
  series_size <- rep(12, length(series_names))
  series_lwidth <- rep(2, length(series_names))
  labels_fp <- rep(list(fp_text(font.size = 0)), length(series_names))
  series_smooth <- rep(1, length(series_names))
  out$series_settings <- list(
    fill = setNames(palette_, series_names),
    colour = setNames(palette_, series_names),
    symbol = setNames(series_symbols, series_names),
    line_style = setNames(series_lstyle, series_names),
    size = setNames(series_size, series_names),
    line_width = setNames(series_lwidth, series_names),
    labels_fp = setNames(labels_fp, series_names),
    smooth = setNames(series_smooth, series_names)
  )

  out
}

#' @title Print method for ms_chart
#' @description An \code{ms_chart} object cannot be rendered
#' in R. The default printing method will only display
#' simple information about the object.
#' If argument \code{preview} is set to TRUE, a \code{pptx} file
#' will be produced and opened with function \code{browseURL}.
#'
#' @param x an \code{ms_chart} object.
#' @param preview preview the chart in a PowerPoint document
#' @param ... unused
#' @return No return value, called for side effects.
#' @export
#' @importFrom officer read_pptx add_slide ph_location_fullsize ph_with
#' @importFrom utils browseURL
print.ms_chart <- function(x, preview = FALSE, ...) {
  if (preview && interactive()) {
    doc <- read_pptx()
    doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
    doc <- ph_with(doc, x, location = ph_location_fullsize())
    file_out <- print(doc, target = tempfile(fileext = ".pptx"))
    browseURL(file_out)
    return(invisible())
  }
  class_val <- setdiff(class(x), "ms_chart")
  cat(sprintf("* %s object\n\n", shQuote(class_val)))

  cat(sprintf("* original data [%.0f,%.0f] (sample):\n", nrow(x$data), ncol(x$data)))
  print(x$data[seq_len(min(c(nrow(x$data), 5))), ])
  cat(sprintf("\n* series data [%.0f,%.0f] (sample):\n", nrow(x$data_series), ncol(x$data_series)))
  print(x$data_series[seq_len(min(c(nrow(x$data_series), 5))), ])
}

colour_list <- list(
  c("#4477AA"),
  c("#4477AA", "#CC6677"),
  c("#4477AA", "#DDCC77", "#CC6677"),
  c("#4477AA", "#117733", "#DDCC77", "#CC6677"),
  c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677"),
  c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677", "#AA4499"),
  c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677", "#AA4499"),
  c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#AA4499"),
  c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"),
  c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
  c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499"),
  c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
)


#' @importFrom htmltools htmlEscape
#' @importFrom xml2 xml_attr<- xml_remove
#' @method format ms_chart
#' @export
format.ms_chart <- function(x, id_x, id_y, sheetname = "sheet1", drop_ext_data = FALSE, ...) {
  str_ <- to_pml(x, id_x = id_x, id_y = id_y, sheetname = sheetname, asis = x$asis, secondary_y = 0)

  if (is.null(x$x_axis$num_fmt)) {
    x$x_axis$num_fmt <- x$theme[[x$fmt_names$x]]
  }
  if (is.null(x$y_axis$num_fmt)) {
    x$y_axis$num_fmt <- x$theme[[x$fmt_names$y]]
  }

  x_axis_str <- axis_content_xml(
    x$x_axis,
    id = id_x,
    theme = x$theme,
    cross_id = id_y,
    is_x = TRUE,
    lab = htmlEscape(x$labels$x),
    rot = x$theme$title_x_rot
  )

  x_axis_str <- sprintf("<%s>%s</%s>", x$axis_tag$x, x_axis_str, x$axis_tag$x)

  y_axis_str <- axis_content_xml(x$y_axis,
    id = id_y, theme = x$theme,
    cross_id = id_x, is_x = FALSE,
    lab = htmlEscape(x$labels$y), rot = x$theme$title_y_rot
  )

  y_axis_str <- sprintf("<%s>%s</%s>", x$axis_tag$y, y_axis_str, x$axis_tag$y)

  secondary_x <- TRUE # logical will become FALSE if secondary axis are created
  secondary_y <- TRUE # logical will become FALSE if secondary axis are created
  ids <- sample(seq.int(60000000, 70000000), size = 4, replace = FALSE)
  if (length(x$secondary)) {

    ser_id <- length(x$yvar) + 1L

    for (sec in seq_along(x$secondary)) {

      is_sec_x <- isTRUE(attr(x$secondary[[sec]], "secondary_x"))
      is_sec_y <- isTRUE(attr(x$secondary[[sec]], "secondary_y"))

      # charts reference their axis via this id
      if (is_sec_y) {
        x_id <- as.character(ids[1])
        y_id <- as.character(ids[2])
      } else if (is_sec_x) {
        x_id <- as.character(ids[3])
        y_id <- as.character(ids[4])
      } else {
        x_id <- id_x
        y_id <- id_y
      }

      # add only one secondary x and y axis if required
      if (secondary_y && is_sec_y) {

        axis_l_str <- axis_content_xml(
          x$secondary[[sec]]$y_axis,
          id = y_id,
          theme = x$secondary[[sec]]$theme,
          cross_id = x_id,
          is_x = FALSE,
          lab = htmlEscape(x$secondary[[sec]]$labels$y),
          rot = x$secondary[[sec]]$theme$title_y_rot
        )

        x_axis_str <- paste0(x_axis_str, sprintf("<%s>%s</%s>", x$secondary[[sec]]$axis_tag$y, axis_l_str, x$secondary[[sec]]$axis_tag$y))

        axis_r_str <- axis_content_xml(
          x$secondary[[sec]]$x_axis,
          id = x_id,
          theme = x$secondary[[sec]]$theme,
          cross_id = y_id,
          is_x = TRUE,
          lab = NULL
        )
        y_axis_str <- paste0(y_axis_str, sprintf("<%s>%s</%s>", x$secondary[[sec]]$axis_tag$x, axis_r_str, x$secondary[[sec]]$axis_tag$x))

        secondary_y <- FALSE

      }

      # add only one secondary x and y axis if required
      if (secondary_x && is_sec_x) {

        axis_l_str <- axis_content_xml(
          x$secondary[[sec]]$y_axis,
          id = y_id,
          theme = x$secondary[[sec]]$theme,
          cross_id = x_id,
          is_x = FALSE,
          lab = NULL,
          rot = x$secondary[[sec]]$theme$title_y_rot
        )

        x_axis_str <- paste0(x_axis_str, sprintf("<%s>%s</%s>", x$secondary[[sec]]$axis_tag$y, axis_l_str, x$secondary[[sec]]$axis_tag$y))

        axis_r_str <- axis_content_xml(
          x$secondary[[sec]]$x_axis,
          id = x_id,
          theme = x$secondary[[sec]]$theme,
          cross_id = y_id,
          is_x = TRUE,
          lab = htmlEscape(x$secondary[[sec]]$labels$x),
        )
        y_axis_str <- paste0(y_axis_str, sprintf("<%s>%s</%s>", x$secondary[[sec]]$axis_tag$x, axis_r_str, x$secondary[[sec]]$axis_tag$x))

        secondary_x <- FALSE

      }

      # all secondary charts
      str_ <- paste0(str_, to_pml(
        x$secondary[[sec]],
        id_y = y_id,
        id_x = x_id,
        sheetname = sheetname,
        asis = x$secondary[[sec]]$asis,
        secondary_y = ser_id
      ))

      ser_id <- ser_id + length(x$secondary[[sec]]$yvar)

    }

  }


  if (inherits(x, "ms_piechart")) {
    x_axis_str <- ""
    y_axis_str <- ""
    table_str <- ""
  } else {
    table_str <- table_content_xml(x)
  }

  sppr_str <- sppr_content_xml(x$theme, "plot")

  ns <- "xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""

  xml_elt <- paste0(
    "<c:plotArea ", ns, "><c:layout/>",
    str_,
    x_axis_str,
    y_axis_str,
    table_str,
    sppr_str,
    "</c:plotArea>"
  )
  xml_doc <- read_xml(system.file(package = "mschart", "template", "chart.xml"))

  node <- xml_find_first(xml_doc, "//c:plotArea")
  xml_replace(node, as_xml_document(xml_elt))

  if (!is.null(x$labels[["title"]])) {
    chartnode <- xml_find_first(xml_doc, "//c:chart")
    title_ <- "<c:title %s><c:tx><c:rich><a:bodyPr/><a:lstStyle/><a:p><a:pPr><a:defRPr/></a:pPr><a:r>%s<a:t>%s</a:t></a:r></a:p></c:rich></c:tx><c:layout/><c:overlay val=\"0\"/></c:title>"
    title_ <- sprintf(title_, ns, format(x$theme[["main_title"]], type = "pml"), htmlEscape(x$labels[["title"]]))
    xml_add_child(chartnode, as_xml_document(title_), .where = 0)
  } else { # null is not enough
    atd_node <- xml_find_first(xml_doc, "//c:chart/c:autoTitleDeleted")
    xml_attr(atd_node, "val") <- "1"
  }

  if (x$theme[["legend_position"]] %in% "n") {
    legend_pos <- xml_find_first(xml_doc, "//c:chart/c:legend")
    xml_remove(legend_pos)
  } else {
    legend_pos <- xml_find_first(xml_doc, "//c:chart/c:legend/c:legendPos")
    xml_attr(legend_pos, "val") <- x$theme[["legend_position"]]

    rpr <- format(x$theme[["legend_text"]], type = "pml")
    rpr <- gsub("a:rPr", "a:defRPr", rpr)
    labels_text_pr <- "<c:txPr xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\" xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"><a:bodyPr/><a:lstStyle/><a:p><a:pPr>%s</a:pPr></a:p></c:txPr>"
    labels_text_pr <- sprintf(labels_text_pr, rpr)
    legend_ <- xml_find_first(xml_doc, "//c:chart/c:legend")
    xml_add_child(legend_, as_xml_document(labels_text_pr))
  }

  chart_area_node <- xml_find_first(xml_doc, "//c:chartSpace")
  chart_area_properties <- sppr_content_xml(x$theme, what = "chart", ns = ns)
  xml_add_child(chart_area_node, as_xml_document(chart_area_properties))

  if (drop_ext_data) {
    xml_remove(xml_find_first(xml_doc, "//c:externalData"))
  }

  as.character(xml_doc)
}
