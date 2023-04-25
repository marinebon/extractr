#' Plot time series
#'
#' The purpose of this function is to generate time series plots of ERRDAP data.
#'
#' @param ts_csv file path to timeseries table as comma-separated value (CSV) (required)
#' @param fld_avg field name containing value average; default = `"mean"`
#' @param fld_sd  field name containing standard deviation average (optional); default = `NULL`
#' @param fld_date field name containing date (required); default = `"date"`
#' @param color color to plot value; default = `"red"`
#' @param label label for y-axes of plot
#' @param ... additional parameters to pass to `dygraphs::dygraph()`
#'
#' @return `dygraphs::dygraph()` object of the time series plot
#' @concept visualize
#' @import dplyr
#' @importFrom dygraphs dygraph dySeries
#' @importFrom readr read_csv
#' @importFrom xts xts
#' @export
#' @examples \dontrun{
#' ts_csv <- here::here("data_tmp/ts.csv")
#' plot_ts(ts_csv, main = "SST")
#' }
#'
plot_ts <- function(
    ts_csv, fld_avg = "mean", fld_sd = NULL, fld_date = "date",
    color = "red", label = "Temperature (°C)", ...){
  # fld_avg = "mean"; fld_sd = "sd"; fld_date = "date"; color = "red"; label = "Temperature (°C)"

  # Read in the csv file
  d <- readr::read_csv(ts_csv, show_col_types = F)

  stopifnot(fld_date %in% colnames(d))
  stopifnot(fld_avg %in% colnames(d))

  d <- d |>
    rename(
      date = all_of(fld_date),
      avg  = all_of(fld_avg)) |>
    arrange(date)
  x <- d |>
    select(avg) |>
    xts::xts(order.by = d$date)

  if (!is.null(fld_sd)){
    stopifnot(fld_sd %in% colnames(d))

    d <- d |>
      rename(
        sd = all_of(fld_sd)) |>
      mutate(
        lwr = avg - sd,
        upr = avg + sd)

    x <- d |>
      select(avg, lwr, upr) |>
      xts::xts(order.by = d$date)
  }

  # TODO:
  # main = "Sea Surface Temperature"

  g <- dygraphs::dygraph(
    x, xlab = "Date", ylab = label, ...)

  if (!is.null(fld_sd)){
    g <- g |>
      dygraphs::dySeries(c("lwr", "avg", "upr"), label = label, color = color)
  } else {
    g <- g |>
      dygraphs::dySeries("avg", label = label, color = color)
  }

  g
}
