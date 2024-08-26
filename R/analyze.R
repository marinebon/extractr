#' Summarize categorical grids into time series table
#'
#' Summarize grids into a table having columns: `date`, `cellvalue` (e.g. Seascape
#' CLASS) and number of cells (`n_cells`).
#'
#' This function is particularly helpful in between using `get_ed_grds()` and
#' `plot_ts_cat()`.
#'
#' @param grds raster stack with more than one date, as returned by
#'   \code{\link{get_ss_grds}}
#' @param ts_csv path to csv to save this time series table. Default is NULL, in
#'   which case the table is not saved. If path is set and already exists then
#'   that will be read in if all dates in the `grds` are present instead of
#'   recalculating and writing to `ts_csv`.
#' @param verbose show messages of process. Useful for debugging. Default:
#'   FALSE.
#'
#' @return `tibble` of data
#' @import dplyr purrr stringr
#' @importFrom tabularaster as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom readr read_csv write_csv cols
#' @export
#' @concept analyze
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#' tbl  <- sum_ss_grds_to_ts(grds)
#' tbl
#'
grds_to_ts_cat <- function(grds, ts_csv = NULL, verbose = F){

  # devtools::load_all()
  # ply  <- get_url_ply("mbnms"); ss_i <- get_ss_info()
  # grds <- get_ss_grds(ss_i, ply, date_beg = "2018-01-01",
  #   dir_tif = here::here("data_ss/mbnms_global_monthly"))
  # ts_csv <- here::here("data_ss/mbnms_global_monthly_CLASS.csv")
  # sum_ss_grds_to_ts(grds, here::here("data_ss/mbnms_global_monthly_CLASS.csv"), verbose = T)

  grds_dates <- tibble(
    date = names(grds) %>%
      str_split("_") %>%
      map(2) %>%
      unlist() %>%
      str_replace_all("[.]", "-") %>%
      as.Date()) %>%
    tibble::rownames_to_column(var = "dimindex") %>%
    mutate(
      dimindex = as.integer(dimindex))

  if (!is.null(ts_csv)){

    if (file.exists(ts_csv)){

      if (verbose)
        message("Reading ts_csv")

      d <- readr::read_csv(ts_csv, col_types = cols())

      grds_dates_match <- grds_dates$date %in% unique(d$date)
      if (all(grds_dates_match)){

        if (!all(unique(d$date) %in% grds_dates$date))
          stop("WHOAH! Haven't handled this condition
               that all dates in `grds` are found in `ts_csv`,
               but not all dates in `ts_csv` are found in `grds`.
               What does this mean?")

        if (verbose)
          message("All grds_dates found in ts_csv$date, so returning vs recalculating.")

        return(d)
      } else {
        if (verbose)
          message(
            glue("Only {sum(grds_dates_match)} of {length(grds_dates_match)} grds_dates found in ts_csv$date, so recalculating."))
      }
    } else {
      if (verbose)
        message(
          glue("The file ts_csv {ts_csv} was not found, so calculating."))
    }
  }

  if (verbose)
    message("Converting raster stack to tibble, then summarizing by date, class.")
  d <- tabularaster::as_tibble(grds) %>%
    left_join(
      grds_dates, by = "dimindex") %>%
    group_by(date, cellvalue) %>%
    summarize(n_cells = n(), .groups = "drop")

  if (!is.null(ts_csv)){
    if (verbose)
      message("Writing to ts_csv.")

    readr::write_csv(d, ts_csv)
  }
  d
}

#' Summarize grids with a continuous variable into a time series table
#'
#' Summarize grids into a table having columns: `date`, `cellvalue` (e.g. Seascape
#' CLASS) and number of cells (`n_cells`).
#'
#' This function is particularly helpful in between using `get_ed_grds()` and
#' `plot_ts_cat()`.
#'
#' @param grds raster stack with more than one date, as returned by
#'   \code{\link{get_ss_grds}}
#' @param ts_csv path to csv to save this time series table. Default is NULL, in
#'   which case the table is not saved. If path is set and already exists then
#'   that will be read in if all dates in the `grds` are present instead of
#'   recalculating and writing to `ts_csv`.
#' @param verbose show messages of process. Useful for debugging. Default:
#'   FALSE.
#'
#' @return `tibble` of data
#' @import dplyr purrr stringr
#' @importFrom tabularaster as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom readr read_csv write_csv cols
#' @export
#' @concept analyze
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#' tbl  <- sum_ss_grds_to_ts(grds)
#' tbl
#'
grds_to_ts <- function(grds, fxns = c("mean", "sd"), ts_csv = NULL, verbose = F){

  # devtools::load_all()
  # ply  <- get_url_ply("mbnms"); ss_i <- get_ss_info()
  # grds <- get_ss_grds(ss_i, ply, date_beg = "2018-01-01",
  #   dir_tif = here::here("data_ss/mbnms_global_monthly"))
  # ts_csv <- here::here("data_ss/mbnms_global_monthly_CLASS.csv")
  # sum_ss_grds_to_ts(grds, here::here("data_ss/mbnms_global_monthly_CLASS.csv"), verbose = T)

  grds_dates <- tibble(
    lyr  = names(grds),
    date = lyr %>%
      str_replace(".*_([0-9]{4}\\.[0-9]{2}\\.[0-9]{2})", "\\1") %>%
      str_replace_all("[.]", "-") %>%
      as.Date())

  if (!is.null(ts_csv)){

    if (file.exists(ts_csv)){

      if (verbose)
        message("Reading ts_csv")

      d <- readr::read_csv(ts_csv, col_types = cols())

      grds_dates_match <- grds_dates$date %in% unique(d$date)
      if (all(grds_dates_match)){

        if (!all(unique(d$date) %in% grds_dates$date))
          stop("WHOAH! Haven't handled this condition
               that all dates in `grds` are found in `ts_csv`,
               but not all dates in `ts_csv` are found in `grds`.
               What does this mean?")

        if (verbose)
          message("All grds_dates found in ts_csv$date, so returning vs recalculating.")

        return(d)
      } else {
        if (verbose)
          message(
            glue("Only {sum(grds_dates_match)} of {length(grds_dates_match)} grds_dates found in ts_csv$date, so recalculating."))
      }
    } else {
      if (verbose)
        message(
          glue("The file ts_csv {ts_csv} was not found, so calculating."))
    }
  }

  d <- grds_dates

  get_lyr_fxn <- function(lyr){
    if (verbose)
      message(glue("{lyr} - {fxn}"))

    terra::global(grds[[lyr]], fxn, na.rm = T)[1,1]
  }

  for (fxn in fxns){ # fxn = fxns[1]

    # OLD: for some reason this now errors out with terra::global()
    #   "error: [global] spatraster has no values"
    # d[fxn] = terra::global( terra::rast(grds), fxn, na.rm = T)
    # NEW: get stats per rast

    d <- d |>
      mutate(
        "{fxn}" := map_dbl(lyr, get_lyr_fxn))
  }

  if (!is.null(ts_csv)){
    if (verbose)
      message("Writing to ts_csv.")

    readr::write_csv(d, ts_csv)
  }
  d
}
