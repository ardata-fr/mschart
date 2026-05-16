# Style helpers for chartEx (cx:) charts. Wrappers thin enough that
# heavy lifting lives in officer (`solid_fill()`, `to_pml.sp_line()`,
# `format(fp_text, type="pml")`). Shape-property fragments are namespaced
# `a:` (DrawingML) and reused inside chartEx wrappers `<cx:spPr>`,
# `<cx:dataPt>`, `<cx:txPr>`, `<cx:title>`.

# --- Series fill dispatch (business logic, kept here) ---------------------

# Returns the category column name in `data_series` against which a
# named-vector `chart_data_fill(values=c(name=color, ...))` matches.
# Histogram is fill-by-bin and Office computes bins itself, so per-point
# fill is not supported there - the method returns NA and only single
# series-wide color works.
cx_fill_cat_column <- function(x) UseMethod("cx_fill_cat_column")
#' @exportS3Method
cx_fill_cat_column.default <- function(x) x$x
#' @exportS3Method
cx_fill_cat_column.ms_treemapchart <- function(x) x$path[length(x$path)]
#' @exportS3Method
cx_fill_cat_column.ms_sunburstchart <- function(x) x$path[length(x$path)]
#' @exportS3Method
cx_fill_cat_column.ms_histogramchart <- function(x) NA_character_
# Pareto with aggregate=TRUE reorders bars by count -> per-point match
# by name would land on the wrong bar. Only single-color is meaningful.
#' @exportS3Method
cx_fill_cat_column.ms_paretochart <- function(x) {
  if (isTRUE(x$aggregate)) NA_character_ else x$x
}

# Layouts whose series spPr line is reused by Excel/PowerPoint to draw
# the connector lines between bars. For these, we must avoid the
# uniform-stroke shortcut (which sets <cx:spPr><a:ln/></cx:spPr> at the
# series level) and emit per-point <cx:dataPt> strokes instead, so the
# connectors keep the chartstyle default.
cx_uses_connectors <- function(x) UseMethod("cx_uses_connectors")
#' @exportS3Method
cx_uses_connectors.default <- function(x) FALSE
#' @exportS3Method
cx_uses_connectors.ms_waterfallchart <- function(x) TRUE

# Build the per-point fill XML fragment for a chartEx series. Reads
# x$cx_fill_per_point (character vector of length nrow(data_series),
# NA for "no override"). Returns "" if no overrides, an inline
# <cx:spPr> if all rows share one color, or a sequence of <cx:dataPt>
# elements otherwise.
cx_render_series_fill <- function(x) {
  vf <- x$cx_fill_per_point
  vs <- x$cx_stroke_per_point
  width <- x$cx_stroke_width %||% 0.75
  if (is.null(vf) && is.null(vs)) {
    return(list(spPr = "", dataPts = ""))
  }
  n <- max(length(vf), length(vs))
  if (is.null(vf)) {
    vf <- rep(NA_character_, n)
  }
  if (is.null(vs)) {
    vs <- rep(NA_character_, n)
  }
  has_any <- !is.na(vf) | !is.na(vs)
  if (!any(has_any)) {
    return(list(spPr = "", dataPts = ""))
  }

  # Series-wide shortcut: every row has the same fill AND same stroke.
  # Layouts with connector lines (waterfall) must keep stroke per-point,
  # otherwise Excel reuses the series spPr line for the connectors.
  has_connectors <- cx_uses_connectors(x)
  uniform_fill <- all(!is.na(vf)) && length(unique(vf)) == 1L
  uniform_stroke <- !has_connectors &&
    all(!is.na(vs)) &&
    length(unique(vs)) == 1L
  fill_only_uniform <- uniform_fill && all(is.na(vs))
  stroke_only_uniform <- uniform_stroke && all(is.na(vf))
  both_uniform <- uniform_fill && uniform_stroke
  if (fill_only_uniform) {
    return(list(spPr = cx_spPr(fill = vf[1]), dataPts = ""))
  }
  if (stroke_only_uniform) {
    return(list(
      spPr = cx_spPr(line_color = vs[1], line_width = width),
      dataPts = ""
    ))
  }
  if (both_uniform) {
    return(list(
      spPr = cx_spPr(fill = vf[1], line_color = vs[1], line_width = width),
      dataPts = ""
    ))
  }

  pts <- vapply(
    which(has_any),
    function(i) {
      cx_dataPt(
        i - 1L,
        fill = if (is.na(vf[i])) NULL else vf[i],
        line_color = if (is.na(vs[i])) NULL else vs[i],
        line_width = width
      )
    },
    character(1)
  )
  list(spPr = "", dataPts = paste0(pts, collapse = ""))
}

# Local duplicates of officer's internal hex_color() / is_transparent().
hex_color <- function(color) {
  rgba <- as.vector(grDevices::col2rgb(color, alpha = TRUE)) / c(1, 1, 1, 255)
  sprintf("%02X%02X%02X", rgba[1], rgba[2], rgba[3])
}

is_transparent <- function(color) {
  if (identical(color, "transparent")) {
    return(TRUE)
  }
  alpha <- as.vector(grDevices::col2rgb(color, alpha = TRUE))[4] / 255
  alpha <= 0
}

# Normalize a color (named, "#RRGGBB", "RRGGBB", NA, NULL, "transparent")
# to uppercase 6-digit hex without "#". Returns NULL for NULL/NA/empty/
# transparent inputs. Errors on unparseable color strings.
cx_color_hex <- function(color) {
  if (is.null(color) || length(color) == 0L) {
    return(NULL)
  }
  if (is.na(color) || !nzchar(color)) {
    return(NULL)
  }
  if (identical(color, "transparent")) {
    return(NULL)
  }
  if (grepl("^[0-9A-Fa-f]{6}$", color)) {
    color <- paste0("#", color)
  }
  hex <- tryCatch(hex_color(color), error = function(e) NULL)
  if (is.null(hex)) {
    stop("invalid color: ", shQuote(color), call. = FALSE)
  }
  if (is_transparent(color)) {
    return(NULL)
  }
  toupper(hex)
}

# --- Solid fill / line wrappers (delegate to officer) ---------------------

# Officer's solid_fill always emits a non-empty fragment; we layer a
# null-check on top so `cx_solid_fill(NULL)` and `cx_solid_fill(NA)`
# return "" (used by render-fill/-spPr to short-circuit empty inputs).
cx_solid_fill <- function(color) {
  if (is.null(color) || length(color) == 0L) {
    return("")
  }
  if (is.na(color) || !nzchar(color)) {
    return("")
  }
  if (identical(color, "transparent")) {
    return("")
  }
  # officer::solid_fill -> col2rgb requires '#' for hex strings.
  if (grepl("^[0-9A-Fa-f]{6}$", color)) {
    color <- paste0("#", color)
  }
  officer::solid_fill(color)
}

# Build an <a:ln> via officer::sp_line + to_pml. Accepts either a plain
# (color, width) pair or a pre-built sp_line for advanced usage.
cx_line <- function(color = NULL, width = 0.75, sp_line = NULL) {
  if (is.null(sp_line)) {
    if (is.null(color) || identical(color, "transparent")) {
      return("")
    }
    if (grepl("^[0-9A-Fa-f]{6}$", color)) {
      color <- paste0("#", color)
    }
    sp_line <- officer::sp_line(color = color, lwd = width)
  }
  officer::to_pml(sp_line)
}

# --- chartEx-specific wrappers --------------------------------------------

# <cx:spPr>{fill}{line}</cx:spPr>. Either part can be NULL/missing.
cx_spPr <- function(fill = NULL, line_color = NULL, line_width = 0.75) {
  fill_xml <- if (!is.null(fill)) cx_solid_fill(fill) else ""
  line_xml <- if (!is.null(line_color)) cx_line(line_color, line_width) else ""
  if (!nzchar(fill_xml) && !nzchar(line_xml)) {
    return("")
  }
  paste0("<cx:spPr>", fill_xml, line_xml, "</cx:spPr>")
}

# <cx:dataPt idx="N"><cx:spPr>...</cx:spPr></cx:dataPt>. Per-point
# overrides for a chartEx series. Note PowerPoint accepts <cx:dataPt>
# (ECMA-376) and <cx:dPt> identically; we use the
# ECMA name. See `?ms_treemapchart` for the hierarchical-rendering
# limitation in PowerPoint.
cx_dataPt <- function(idx, fill = NULL, line_color = NULL, line_width = 0.75) {
  spPr <- cx_spPr(fill, line_color, line_width)
  if (!nzchar(spPr)) {
    return("")
  }
  sprintf("<cx:dataPt idx=\"%d\">%s</cx:dataPt>", as.integer(idx), spPr)
}

# --- Text properties via officer's fp_text -> PresentationML --------------

# Build an <a:rPr> (or <a:defRPr>) block from an fp_text by reusing
# officer's canonical converter (`format(fp, type="pml")`). The returned
# fragment includes solidFill (with alpha), latin/cs/ea/sym typefaces
# and all sz/b/i/u/strike/cap attributes. `as_def` rewrites the tag to
# <a:defRPr> for use inside <a:pPr> (txPr context).
cx_rPr_from_fp_text <- function(fp, as_def = FALSE) {
  if (is.null(fp)) {
    return("")
  }
  out <- format(fp, type = "pml")
  if (isTRUE(as_def)) {
    out <- gsub("a:rPr", "a:defRPr", out, fixed = TRUE)
  }
  out
}

# <cx:txPr> for axis/legend/dataLabels styling. Style is carried by
# <a:defRPr> inside an empty paragraph (text content itself comes from
# data/category names).
cx_txPr_from_fp_text <- function(fp) {
  if (is.null(fp)) {
    return("")
  }
  defRPr <- cx_rPr_from_fp_text(fp, as_def = TRUE)
  # PowerPoint (notably Mac) reads endParaRPr to color/size auto-rendered
  # text such as data label values. Mirror the styling on both defRPr and
  # endParaRPr for parity.
  endRPr <- gsub(
    "a:rPr",
    "a:endParaRPr",
    cx_rPr_from_fp_text(fp, as_def = FALSE),
    fixed = TRUE
  )
  paste0(
    "<cx:txPr>",
    "<a:bodyPr/>",
    "<a:lstStyle/>",
    "<a:p><a:pPr>",
    defRPr,
    "</a:pPr>",
    endRPr,
    "</a:p>",
    "</cx:txPr>"
  )
}

# <cx:dataLabels [pos="..."]><cx:visibility .../><cx:numFmt .../><cx:txPr .../></cx:dataLabels>
# Per-layout defaults are passed in `defaults` (list with position, show_val,
# show_cat, show_serie). User overrides via x$cx_data_labels (`opts`).
# `fp` is an fp_text for label font (optional).
# Returns "" when both defaults and opts are NULL (chart with no labels).
cx_data_labels_xml <- function(
  defaults = NULL,
  opts = NULL,
  fp = NULL,
  n_points = NULL
) {
  if (is.null(defaults) && is.null(opts)) {
    return("")
  }
  d <- defaults %||% list()
  o <- opts %||% list()
  pos <- o$position %||% d$position
  show_val <- isTRUE(o$show_val %||% d$show_val)
  show_cat <- isTRUE(o$show_cat %||% d$show_cat)
  show_ser <- isTRUE(o$show_serie %||% d$show_serie)
  num_fmt <- o$num_fmt
  pos_attr <- if (is.null(pos) || !nzchar(pos)) {
    ""
  } else {
    sprintf(" pos=\"%s\"", pos)
  }
  vis_xml <- sprintf(
    "<cx:visibility seriesName=\"%d\" categoryName=\"%d\" value=\"%d\"/>",
    as.integer(show_ser),
    as.integer(show_cat),
    as.integer(show_val)
  )
  numfmt_xml <- if (is.null(num_fmt) || !nzchar(num_fmt)) {
    ""
  } else {
    sprintf(
      "<cx:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>",
      htmltools::htmlEscape(num_fmt)
    )
  }
  # The bundled chartstyle (style1.xml) often assigns labels a near-white
  # text color (designed for inEnd labels on dark bars). Without an
  # explicit override the labels become invisible on light backgrounds.
  # Force a sensible default when the caller didn't supply an fp_text.
  if (is.null(fp)) {
    fp <- officer::fp_text(font.size = 10, color = "black")
  }
  txpr_xml <- cx_txPr_from_fp_text(fp)
  # Per-point <cx:dataLabel idx="N"> blocks. Required by some PowerPoint
  # versions for layouts (notably waterfall) that do NOT honor the
  # series-level <cx:visibility> alone.
  perpt_xml <- ""
  if (!is.null(n_points) && n_points > 0L) {
    pp_pos <- if (is.null(pos) || !nzchar(pos)) {
      ""
    } else {
      sprintf(" pos=\"%s\"", pos)
    }
    perpt_xml <- paste0(
      vapply(
        seq_len(n_points) - 1L,
        function(i) {
          sprintf(
            "<cx:dataLabel idx=\"%d\"%s>%s</cx:dataLabel>",
            i,
            pp_pos,
            vis_xml
          )
        },
        character(1)
      ),
      collapse = ""
    )
  }
  paste0(
    sprintf("<cx:dataLabels%s>", pos_attr),
    vis_xml,
    numfmt_xml,
    txpr_xml,
    perpt_xml,
    "</cx:dataLabels>"
  )
}

# <cx:valScaling [min="..."] [max="..."]/>
cx_val_scaling <- function(min = NULL, max = NULL, extra_attrs = "") {
  attrs <- character()
  if (!is.null(min)) {
    attrs <- c(
      attrs,
      sprintf("min=\"%s\"", format(min, scientific = FALSE, trim = TRUE))
    )
  }
  if (!is.null(max)) {
    attrs <- c(
      attrs,
      sprintf("max=\"%s\"", format(max, scientific = FALSE, trim = TRUE))
    )
  }
  if (nzchar(extra_attrs)) {
    attrs <- c(attrs, extra_attrs)
  }
  if (length(attrs)) {
    sprintf("<cx:valScaling %s/>", paste(attrs, collapse = " "))
  } else {
    "<cx:valScaling/>"
  }
}

# <cx:axis id="N">{scaling}{extras}<cx:tickLabels/>{txPr}</cx:axis>.
# `scaling` is the cat/val scaling element (raw XML string). `extras`
# can carry majorGridlines, units, etc. `fp` is an fp_text controlling
# tick label font (typically theme$axis_text_x or theme$axis_text_y).
# Order respects CT_Axis schema: scaling, ..., tickLabels, txPr.
cx_axis <- function(
  id,
  scaling,
  extras = "",
  fp = NULL,
  title = NULL,
  title_fp = NULL,
  major_grid = FALSE,
  minor_grid = FALSE,
  num_fmt = NULL
) {
  # Axis titles use <cx:title> without pos/align/overlay attributes
  # (those would be invalid in axis context per chartEx schema; position
  # is implicit and tied to the parent axis).
  title_xml <- if (is.null(title) || !nzchar(title)) {
    ""
  } else {
    paste0("<cx:title>", cx_rich_text(title, fp = title_fp), "</cx:title>")
  }
  major_xml <- if (isTRUE(major_grid)) "<cx:majorGridlines/>" else ""
  minor_xml <- if (isTRUE(minor_grid)) "<cx:minorGridlines/>" else ""
  numfmt_xml <- if (is.null(num_fmt) || !nzchar(num_fmt)) {
    ""
  } else {
    sprintf(
      "<cx:numFmt formatCode=\"%s\" sourceLinked=\"0\"/>",
      htmltools::htmlEscape(num_fmt)
    )
  }
  # CT_Axis order: scaling, title, units, majorGridlines, minorGridlines,
  # majorTickMarks, minorTickMarks, tickLabels, numFmt, spPr, txPr.
  paste0(
    sprintf("<cx:axis id=\"%s\">", id),
    scaling,
    title_xml,
    extras,
    major_xml,
    minor_xml,
    "<cx:tickLabels/>",
    numfmt_xml,
    cx_txPr_from_fp_text(fp),
    "</cx:axis>"
  )
}

# <cx:title pos="t" align="ctr" overlay="0">...</cx:title>. Returns ""
# when text is NULL/empty. The title text inherits font/color/size from
# the supplied fp_text (typically theme$main_title).
cx_chart_title <- function(text, fp = NULL) {
  if (is.null(text) || !nzchar(text)) {
    return("")
  }
  paste0(
    "<cx:title pos=\"t\" align=\"ctr\" overlay=\"0\">",
    cx_rich_text(text, fp = fp),
    "</cx:title>"
  )
}

# <cx:tx><cx:rich>...</cx:rich></cx:tx> for titles (chart, axis). The
# run carries an explicit <a:rPr> from fp_text. align: "ctr" (default),
# "l" or "r".
cx_rich_text <- function(text, fp = NULL, align = "ctr") {
  rpr <- cx_rPr_from_fp_text(fp, as_def = FALSE)
  if (!nzchar(rpr)) {
    rpr <- "<a:rPr/>"
  }
  paste0(
    "<cx:tx><cx:rich>",
    "<a:bodyPr/>",
    "<a:lstStyle/>",
    sprintf("<a:p><a:pPr algn=\"%s\"/>", align),
    "<a:r>",
    rpr,
    sprintf("<a:t>%s</a:t>", htmltools::htmlEscape(text)),
    "</a:r></a:p>",
    "</cx:rich></cx:tx>"
  )
}

# Build a <cx:spPr><a:ln .../></cx:spPr> fragment used to draw the
# cumulative line of pareto charts and the whisker/median/mean strokes
# of boxplot charts. Without this override Excel resolves the
# chartstyle phClr placeholder to an invisible color.
#   NULL        -> use scheme accent5 (matches Excel-native defaults)
#   FALSE       -> no spPr (lines left to chartstyle, fragile)
#   fp_border() -> convert via officer's ooxml_fp_border
cx_default_line_spPr <- function(line) {
  if (isFALSE(line)) {
    return("")
  }
  if (is.null(line)) {
    return(paste0(
      "<cx:spPr><a:ln w=\"12700\">",
      "<a:solidFill><a:schemeClr val=\"accent5\"/></a:solidFill>",
      "</a:ln></cx:spPr>"
    ))
  }
  ooxml_fp_border(line, in_tags = c("cx:spPr"))
}
