# Base helpers for chartEx (cx:) charts -- a parallel pipeline to the
# classic c: charts. ms_chart_ex inherits from ms_chart so user-facing
# styling setters that operate on $theme / $labels keep working.

CX_NS <- paste(
  "xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\"",
  "xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\"",
  "xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""
)

# Per-class plumbing for ph_with.ms_chart and body_add_chart.

chart_part_content_type <- function(x) UseMethod("chart_part_content_type")
#' @exportS3Method
chart_part_content_type.ms_chart <- function(x) {
  "application/vnd.openxmlformats-officedocument.drawingml.chart+xml"
}
#' @exportS3Method
chart_part_content_type.ms_chart_ex <- function(x) {
  "application/vnd.ms-office.chartex+xml"
}

chart_part_rel_type <- function(x) UseMethod("chart_part_rel_type")
#' @exportS3Method
chart_part_rel_type.ms_chart <- function(x) {
  "http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart"
}
#' @exportS3Method
chart_part_rel_type.ms_chart_ex <- function(x) {
  "http://schemas.microsoft.com/office/2014/relationships/chartEx"
}

chart_graphicdata_uri <- function(x) UseMethod("chart_graphicdata_uri")
#' @exportS3Method
chart_graphicdata_uri.ms_chart <- function(x) {
  "http://schemas.openxmlformats.org/drawingml/2006/chart"
}
#' @exportS3Method
chart_graphicdata_uri.ms_chart_ex <- function(x) {
  "http://schemas.microsoft.com/office/drawing/2014/chartex"
}

chart_reference_xml <- function(x, rel_id) UseMethod("chart_reference_xml")
#' @exportS3Method
chart_reference_xml.ms_chart <- function(x, rel_id) {
  sprintf(
    paste0(
      "<c:chart xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\"",
      " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"",
      " r:id=\"rId%.0f\"/>"
    ),
    rel_id
  )
}
# Sidecar parts (chart style + colors). chartEx requires both:
# without them PowerPoint cannot render the new layouts (treemap, sunburst,
# funnel, ...) and flags the file as corrupt. Classic c: charts are fine
# without sidecars so the default method writes nothing.
chart_extra_parts <- function(
  value,
  charts_dir,
  content_type,
  part_root = "/ppt/charts"
) {
  UseMethod("chart_extra_parts")
}
#' @exportS3Method
chart_extra_parts.ms_chart <- function(
  value,
  charts_dir,
  content_type,
  part_root = "/ppt/charts"
) {
  ""
}
#' @exportS3Method
chart_extra_parts.ms_chart_ex <- function(
  value,
  charts_dir,
  content_type,
  part_root = "/ppt/charts"
) {
  style_file <- tempfile(
    tmpdir = charts_dir,
    pattern = "style",
    fileext = ".xml"
  )
  colors_file <- tempfile(
    tmpdir = charts_dir,
    pattern = "colors",
    fileext = ".xml"
  )
  file.copy(
    system.file(package = "mschart", "template", "chartex_style.xml"),
    style_file,
    overwrite = TRUE
  )
  file.copy(
    system.file(package = "mschart", "template", "chartex_colors.xml"),
    colors_file,
    overwrite = TRUE
  )
  content_type$add_override(
    value = setNames(
      "application/vnd.ms-office.chartstyle+xml",
      file.path(part_root, basename(style_file))
    )
  )
  content_type$add_override(
    value = setNames(
      "application/vnd.ms-office.chartcolorstyle+xml",
      file.path(part_root, basename(colors_file))
    )
  )
  paste0(
    sprintf(
      "<Relationship Id=\"rId2\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartStyle\" Target=\"%s\"/>",
      basename(style_file)
    ),
    sprintf(
      "<Relationship Id=\"rId3\" Type=\"http://schemas.microsoft.com/office/2011/relationships/chartColorStyle\" Target=\"%s\"/>",
      basename(colors_file)
    )
  )
}

#' @exportS3Method
chart_reference_xml.ms_chart_ex <- function(x, rel_id) {
  sprintf(
    paste0(
      "<cx:chart xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\"",
      " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"",
      " r:id=\"rId%.0f\"/>"
    ),
    rel_id
  )
}

# Build the full chartEx XML for a hierarchical chart (treemap, sunburst).
# Both layouts use the same data shape: multi-level <cx:strDim type="cat">
# (leaf-first) + <cx:numDim type="size">. Differences are layoutId and
# layout-specific <cx:layoutPr> children.
cx_format_hierarchical <- function(
  x,
  layout_id,
  sheetname,
  drop_ext_data,
  series_layout_pr = ""
) {
  ds <- x$data_series
  n <- nrow(ds)
  path <- x$path
  value <- x$value

  path_cols <- xl_col_letter(seq_along(path))
  value_col <- xl_col_letter(length(path) + 1L)
  row1 <- 2L
  row2 <- n + 1L

  # Office orders levels leaf-first (lvl[0] = leaf, lvl[n-1] = root).
  cat_levels_xml <- paste0(
    vapply(
      rev(seq_along(path)),
      function(i) {
        cx_str_lvl(ds[[path[i]]])
      },
      character(1)
    ),
    collapse = ""
  )
  cat_range <- if (length(path) == 1L) {
    cx_range(sheetname, path_cols[1], row1, row2)
  } else {
    sprintf(
      "%s!$%s$%d:$%s$%d",
      sheetname,
      path_cols[1],
      row1,
      path_cols[length(path)],
      row2
    )
  }
  cat_dim <- paste0(
    "<cx:strDim type=\"cat\">",
    sprintf("<cx:f>%s</cx:f>", cat_range),
    cat_levels_xml,
    "</cx:strDim>"
  )

  val_dim <- paste0(
    "<cx:numDim type=\"size\">",
    sprintf("<cx:f>%s</cx:f>", cx_range(sheetname, value_col, row1, row2)),
    cx_num_lvl(ds[[value]], format_code = "Standard"),
    "</cx:numDim>"
  )

  unique_id <- x$unique_id %||% cx_unique_id()
  fill <- cx_render_series_fill(x)
  series_xml <- paste0(
    sprintf(
      "<cx:series layoutId=\"%s\" uniqueId=\"%s\">",
      layout_id,
      unique_id
    ),
    "<cx:tx><cx:txData>",
    sprintf("<cx:f>%s</cx:f>", cx_cell(sheetname, value_col, 1L)),
    sprintf("<cx:v>%s</cx:v>", htmltools::htmlEscape(value)),
    "</cx:txData></cx:tx>",
    fill$spPr,
    fill$dataPts,
    cx_data_labels_xml(
      defaults = list(
        position = "ctr",
        show_cat = TRUE,
        show_val = FALSE,
        show_serie = FALSE
      ),
      opts = x$cx_data_labels,
      fp = x$cx_data_labels$fp
    ),
    "<cx:dataId val=\"0\"/>",
    series_layout_pr,
    "</cx:series>"
  )

  title_xml <- cx_chart_title(x$labels[["title"]], fp = x$theme$main_title)

  ext_data_xml <- if (drop_ext_data) {
    ""
  } else {
    "<cx:externalData r:id=\"rId1\" cx:autoUpdate=\"0\"/>"
  }

  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<cx:chartSpace",
    " xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"",
    " xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"",
    " xmlns:cx=\"http://schemas.microsoft.com/office/drawing/2014/chartex\">",
    "<cx:chartData>",
    ext_data_xml,
    "<cx:data id=\"0\">",
    cat_dim,
    val_dim,
    "</cx:data>",
    "</cx:chartData>",
    "<cx:chart>",
    title_xml,
    "<cx:plotArea><cx:plotAreaRegion>",
    series_xml,
    "</cx:plotAreaRegion></cx:plotArea>",
    "</cx:chart>",
    "</cx:chartSpace>"
  )
}

# Excel A1 column letters (A, B, ..., Z, AA, ...). Path columns + value
# column live in the first columns of the embedded sheet, in the order
# given by data_series. We only need a small, safe range generator.
xl_col_letter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[k]
    s <- ""
    while (n > 0) {
      r <- (n - 1L) %% 26L
      s <- paste0(LETTERS[r + 1L], s)
      n <- (n - 1L) %/% 26L
    }
    out[k] <- s
  }
  out
}

cx_range <- function(sheet, col, row1, row2) {
  sprintf("%s!$%s$%d:$%s$%d", sheet, col, row1, col, row2)
}

cx_cell <- function(sheet, col, row) {
  sprintf("%s!$%s$%d", sheet, col, row)
}

# Build a <cx:lvl> with inline string points. No formatCode (matches
# Excel-generated chartEx for string dimensions).
cx_str_lvl <- function(values) {
  pts <- paste0(
    sprintf(
      "<cx:pt idx=\"%d\">%s</cx:pt>",
      seq_along(values) - 1L,
      htmltools::htmlEscape(values)
    ),
    collapse = ""
  )
  sprintf("<cx:lvl ptCount=\"%d\">%s</cx:lvl>", length(values), pts)
}

# Build a <cx:lvl> with inline numeric points.
cx_num_lvl <- function(values, format_code = "General") {
  vals <- format(values, scientific = FALSE, trim = TRUE)
  vals[is.na(values)] <- ""
  pts <- paste0(
    sprintf("<cx:pt idx=\"%d\">%s</cx:pt>", seq_along(values) - 1L, vals),
    collapse = ""
  )
  sprintf(
    "<cx:lvl ptCount=\"%d\" formatCode=\"%s\">%s</cx:lvl>",
    length(values),
    format_code,
    pts
  )
}

# Generate a chartEx-style uniqueId GUID. Office uses uppercase hex
# wrapped in braces.
cx_unique_id <- function() {
  hex <- function(n) {
    paste(sprintf("%X", sample.int(16L, n, replace = TRUE) - 1L), collapse = "")
  }
  sprintf("{%s-%s-%s-%s-%s}", hex(8), hex(4), hex(4), hex(4), hex(12))
}
