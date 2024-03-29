% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_chart.R
\name{ms_areachart}
\alias{ms_areachart}
\title{areachart object}
\usage{
ms_areachart(data, x, y, group = NULL, labels = NULL, asis = FALSE)
}
\arguments{
\item{data}{a data.frame}

\item{x}{x colname}

\item{y}{y colname}

\item{group}{grouping colname used to split data into series. Optional.}

\item{labels}{colnames of columns to be used as labels into series. Optional.
If more than a name, only the first one will be used as label, but all
labels (transposed if a group is used) will be available in the Excel file
associated with the chart.}

\item{asis}{bool parameter defaulting to FALSE. If TRUE the data will not be
modified.}
}
\description{
Creation of an areachart object that can be
inserted in a 'Microsoft' document.

Area charts can be used to plot change over time and draw attention to the
total value across a trend. By showing the sum of the plotted values, an area
chart also shows the relationship of parts to a whole.
}
\examples{
library(officer)
mytheme <- mschart_theme(
  axis_title_x = fp_text(color = "red", font.size = 24, bold = TRUE),
  axis_title_y = fp_text(color = "green", font.size = 12, italic = TRUE),
  grid_major_line_y = fp_border(width = 1, color = "orange"),
  axis_ticks_y = fp_border(width = 1, color = "orange")
)

# example ac_01 -------
ac_01 <- ms_areachart(
  data = browser_ts, x = "date",
  y = "freq", group = "browser"
)
ac_01 <- chart_ax_y(ac_01, cross_between = "between", num_fmt = "General")
ac_01 <- chart_ax_x(ac_01, cross_between = "midCat", num_fmt = "m/d/yy")
ac_01 <- set_theme(ac_01, mytheme)


# example ac_02 -------
ac_02 <- chart_settings(ac_01, grouping = "percentStacked")

# example ac_03 -------
ac_03 <- chart_settings(ac_01, grouping = "percentStacked", table = TRUE)
ac_03 <- chart_table(
  ac_03,
  horizontal = FALSE, vertical = FALSE,
  outline = FALSE, show_keys = TRUE)
}
\seealso{
\code{\link[=chart_settings]{chart_settings()}}, \code{\link[=chart_ax_x]{chart_ax_x()}}, \code{\link[=chart_ax_y]{chart_ax_y()}},
\code{\link[=chart_data_labels]{chart_data_labels()}}, \code{\link[=chart_theme]{chart_theme()}}, \code{\link[=chart_labels]{chart_labels()}}

Other 'Office' chart objects: 
\code{\link{ms_barchart}()},
\code{\link{ms_linechart}()},
\code{\link{ms_scatterchart}()}
}
\concept{'Office' chart objects}
