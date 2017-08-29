#' @title ms_chart object
#' @description Creation of a chart object that can be
#' inserted in a 'Microsoft' document.
#' @param data a data.frame
#' @param x x colname
#' @param y y colname
#' @param group grouping colname used to split data into series. Optional.
#' @name mschart
#' @rdname mschart
NULL

#' Dummy dataset for barchart
#'
#' A dataset containing 2 categorical and an integer variables:
#'
#' \itemize{
#'   \item browser web browser
#'   \item serie id of series
#'   \item value integer values
#' }
#'
#' @name browser_data
#' @docType data
#' @keywords datasets
#' @usage data(browser_data)
#' @format A data frame with 18 rows and 3 variables
NULL

#' Dummy dataset for barchart
#'
#' A dataset containing a date, a categorical and an integer variables:
#'
#' \itemize{
#'   \item date date values
#'   \item browser web browser
#'   \item freq values in percent
#' }
#'
#' @name browser_ts
#' @docType data
#' @keywords datasets
#' @usage data(browser_ts)
#' @format A data frame with 36 rows and 3 variables
NULL
