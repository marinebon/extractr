#' Create polygon from bounding box
#'
#' Utility function to create polygon from bounding box
#'
#' @param lon_min longitude, minimum
#' @param lat_min latitude, minimum
#' @param lon_max longitude, maximum
#' @param lat_max latitude, maximum
#'
#' @return Returns a spatial feature \link[sf]{sf} polygon.
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sfc
#' @importFrom sf st_bbox
#' @importFrom raster extent
#' @export
#' @concept read
#'
#' @examples
#' # Florida Keys area
#' lon = -81.3; lat = 24.5; w = 10
#' bbox_ply(lon - w, lat - w, lon + w, lat + w)
bbox_ply <- function(lon_min, lat_min, lon_max, lat_max){
  raster::extent(lon_min, lon_max, lat_min, lat_max) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()
}

#' Get polygon from link or NOAA Sanctuary code
#'
#' Given a sanctuary code or link (ie URL), download the zip, unzip it, and read
#' first shapefile as a spatial feature. This function was originally designed
#' to pull from one of the zip links found at
#' \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#' GIS}.
#'
#' @param sanctuary NOAA Sanctuary code with which to form `url`. Sanctuary
#'   codes can be found at
#'   \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#'   GIS}.
#' @param url Link from which to fetch a polygon. Originally designed to pull
#'   from one of the zip links found at
#'   \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#'   GIS}.
#' @param dir_ply Directory to download locally into. This directory serves as
#'   a cache to skip operations if already performed. If zip file exists, skip
#'   downloading. If directory to unzip exists, skip unzipping.
#' @param verbose Verbose meedages describing operations and choices, such as
#'   skipping download or unzip and which shapefile used if more than one found.
#'
#' @return Returns a spatial feature \link[sf]{sf} polygon data frame.
#' @importFrom fs path path_ext_remove
#' @importFrom R.utils isUrl
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom sf read_sf st_union st_transform
#' @export
#' @concept read
#'
#' @examples
#' # mbnms: Monterey Bay National Marine Sanctuary
#' ply_mbnms <- get_url_ply(sanctuary = "mbnms")
#' ply_mbnms
#' plot(ply_mbnms[1])
#'
#' # fknms: Florida Keys National Marine Sanctuary
#' ply_fknms <- get_url_ply(sanctuary = "fknms")
#' ply_fknms
#' plot(ply_fknms[1])
get_url_ply <- function(
  sanctuary = NULL,
  url       = NULL,
  dir_ply   = here::here("data_ed/ply"),
  verbose   = F){
  # url = "https://sanctuaries.noaa.gov/library/imast/mbnms_py2.zip"
  # dir_ply = here::here("data_seascapeR")

  # TODO: handle error with Papahānaumokuākea
  #   ply = get_url_ply("pmnm); ed_info <- ed_info();
  #   get_ed_grds(ed_info, ply, date_beg = "2019-01-01", date_end = "2020-01-01")
  #     ERROR: One or both longitude values (-180, 180) outside data range (-179.975, 179.975)

  if (is.null(sanctuary) & is.null(url))
    stop("Please provide a url or sanctuary argument")

  if (!is.null(sanctuary))
    url <- case_when(
      sanctuary == "mbpr" ~
        glue("https://sanctuaries.noaa.gov/media/gis/{sanctuary}_py.zip"),
      sanctuary == "pmnm" ~
        glue("https://sanctuaries.noaa.gov/library/imast/{sanctuary}_py.zip"),
      T ~
        glue("https://sanctuaries.noaa.gov/library/imast/{sanctuary}_py2.zip"))

  if(!R.utils::isUrl(url))
    stop(glue("The url '{url}' does not appear to be a URL."))

  dir_create(dir_ply)

  f <- path(dir_ply, basename(url))
  if (file.exists(f)){
    if (verbose)
      message(glue("
      Found file: {f}
        Skipping download: {url}"))
  } else {
    if (verbose)
      message(glue("
      Downloading
        from: {url}
        to:   {f}"))
    download.file(url, f)
  }

  if (path_ext(f) == "zip"){
    dir_unzip <- fs::path(dir_ply, fs::path_ext_remove(basename(f)))
    if (dir_exists(dir_unzip)){
      if (verbose)
        message(glue("
        Found dir: {dir_unzip}
          Skipping unzip of file: {f}"))
    } else {
      if (verbose)
        message(glue("
        Unzipping
          from: {f}
          to:   {dir_unzip}"))
      unzip(f, exdir = dir_unzip)
    }
    dir_shp <- dir_unzip
  } else {
    dir_shp <- dirname(f)
  }

  shps <- list.files(dir_shp, ".*\\.shp$", recursive = T, full.names = T)
  if (length(shps) == 0)
    stop(glue("No shapefiles (*.shp) found in {dir_shp}"))
  if (length(shps) > 1)
    message(glue("
    More than one shapefile (*.shp) found in {dir_shp}
      Reading only last: {basename(shps[length(shps)])}")) # Want last of: c(PMNM_py_Albers.shp, PMNM_py.shp)

  if (verbose)
    message(glue("
    Reading spatial features from
      shapefile: {shps[length(shps)]}"))

  suppressMessages({
    ply <- sf::read_sf(shps[length(shps)]) %>%
      sf::st_transform(crs = wgs84) %>%
      sf::st_union()
  })
  ply
}

#' Get Seascape grids within polygon for date range
#'
#' Given a polygon and date range, fetch Seascape data and return a raster layer
#' for a single date or raster stack if multiple dates found for given date
#' range.
#'
#' @param ed_info SeaScape ERDDAP info object, as returned by
#'   \code{\link{ed_info}})
#' @param ed_var SeaScape variable. One of "CLASS" (default) or "P" for
#'   probability.
#' @param ply polygon as spatial feature \code{\link[sf]{sf}}, as returned by
#'   \code{\link{get_url_ply}} or \code{\link{bbox_to_ply}}
#' @param date_beg date begin to fetch, as character (`"2003-01-15"`) or Date
#'   (`Date("2003-01-15")`). Defaults to first date available from `ed_info`.
#' @param date_end date end to fetch, as character (`"2020-11-15"`) or Date
#'   (`Date("2020-11-15")`). Defaults to latest date available from `ed_info`.
#' @param dir_tif directory to cache results. Files are stored in the format
#'   `grd_{ed_var}_{date}.tif` so any other information needed, such as place or
#'   SeaScape dataset should be captured by the containing folders so as to not
#'   inadvertently write or read the wrong grid. This folder is consulted for
#'   available dates before fetching any missing from the ERDDAP server.
#' @param del_cache Delete ERDDAP cache with
#'   \code{\link[rerddap]{cache_delete_all}}, which may be necessary if
#'   underlying data changed. Default: FALSE.
#' @param verbose display messages on status of function. Useful for debugging
#'   or showing status while getting data from a wide range and/or big polygon.
#'   Default: FALSE.
#'
#' @return Raster \code{\link[raster]{raster}} layer if one date,
#'   \code{\link[raster]{stack}} if more
#' @import lubridate purrr sf tidyr
#' @importFrom glue glue
#' @importFrom raster raster
#' @importFrom raster crs
#' @importFrom raster mask
#' @importFrom raster plot
#' @importFrom sp coordinates
#' @importFrom sp gridded
#' @export
#' @concept read
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ed_i <- ed_info()
#' grds <- get_ed_grds(ed_i, ply, date_beg = "2020-01-01")
#' grds
#'
get_ed_grds <- function(
  ed_info,
  ply,
  ed_var    = "CLASS",
  date_beg  = min(ed_dim(ed_info, "time")),
  date_end  = max(ed_dim(ed_info, "time")),
  dir_tif   = NULL,
  del_cache = F,
  verbose   = T){
  # ed_info = ed; date_beg = "2023-04-20"; date_end = "2023-04-24"
  # del_cache = F; verbose   = T

  select = dplyr::select

  if (del_cache)
    rerddap::cache_delete_all(force = T)

  s_dates <- ed_dim(ed_info, "time")

  date_beg <- as.Date(date_beg)
  date_end <- as.Date(date_end)

  if (!lubridate::int_overlaps(
    lubridate::interval(  date_beg, date_end  ),
    lubridate::interval(s_dates[1], s_dates[2]))){
    stop(glue("Date range requested ({date_beg} to {date_end}) does not overlap with Seascapes ({s_dates[1]} to {s_dates[2]})."))
  }

  if (date_end > s_dates[2]){
    warning(glue("The date_end {date_end} > Seascapes end ({s_dates[2]}) so decreasing to {s_dates[2]}."))
    date_end <- s_dates[2]
  }

  if (date_beg < s_dates[1]){
    warning(glue("The date_beg {date_beg} < Seascape begin ({s_dates[1]}) so increasing to {s_dates[1]}."))
    date_beg <- s_dates[1]
  }

  # dates on ERDDAP
  dates_all <- ed_dim(ed_info, "time")

  if (verbose)
    message(glue("Found {length(dates_all)} dates between {date_beg} and {date_end}."))

  # TODO: handle error with Papahānaumokuākea
  #   ply = get_url_ply("pmnm); ed_info <- ed_info();
  #   get_ed_grds(ed_info, ply, date_beg = "2019-01-01", date_end = "2020-01-01")
  #     ERROR: One or both longitude values (-180, 180) outside data range (-179.975, 179.975)

  # ed_info   = ed_info()
  # ply       = get_url_ply("mbnms")
  # ed_var    = "CLASS"
  # date_beg  = "2019-01-01"; date_end  = "2021-01-01"
  # dir_tif   = here("data_ed/mbnms_global_monthly_2020-01-01_to_2021-01-01")
  # write_tif = T

  if (!is.null(dir_tif)){
    dir_create(dir_tif)

    tifs <- tibble(
      tif = list.files(
        path = dir_tif,
        pattern=glue('grd_{ed_var}_.*tif$'),
        recursive = T, full.names=T)) %>%
      mutate(
        date_chr = map_chr(tif, function(x){
          basename(x) %>%
            str_replace(glue('grd_{ed_var}_(.*)\\.tif$'), "\\1") %>%
            str_replace_all(fixed("."), "-")}),
        date = as.Date(date_chr))

    tifs_match <- tifs %>%
      filter(date %in% dates_all)

    if (verbose)
      message(glue("Found {nrow(tifs_match)} matching tifs of {length(dates_all)} dates."))

    if (nrow(tifs_match) > 0)
      tbl_tifs <- tifs_match %>%
      mutate(
        raster = map(
          tif, raster::raster))

    if (all(dates_all %in% tifs$date)){
      if (verbose)
        message(
          glue("Reading existing grids ([dir_tif]/grd_{ed_var}_[date].tif) vs fetching fresh data via ERDDAP."))

      if (nrow(tifs_match) == 1)
        grd <- tbl_tifs$raster[[1]]
      if (nrow(tifs_match) > 1)
        grd <- raster::stack(tbl_tifs$raster)

      names(grd) <- names(grd) %>% str_replace("^grd_", "")
      return(grd)
    } else {
      dates_get <- setdiff(dates_all, tifs$date) %>% as.Date(origin="1970-01-01")
    }
  } else {
    dates_get <- dates_all
  }

  # if (verbose)
  #   message(glue("Proceeding to fetch {length(dates_get)} from ERDDAP of {length(dates_all)} dates."))

  bb <- sf::st_bbox(ply)
  # TODO: check bb's crs==4326 and within range of dataset product

  get_ed_raster <- function(date){
    if (verbose)
      message(glue("  griddap({date}, ...)"))

    nc <- try(griddap(
      datasetx  = attr(ed_info, "datasetid"),
      fields    = ed_var,
      url       = ed_info$base_url,
      longitude = c(bb["xmin"], bb["xmax"]),
      latitude  = c(bb["ymin"], bb["ymax"]),
      time      = c(date, date) |> as.character(),
      fmt       = "nc"))

    if ("try-error" %in% class(nc)){
      stop(glue("
        Problem fetching data from ERDDAP server using:
          rerddap::griddap(
            datasetx  = '{attr(ed_info, 'datasetid')}',
            fields    = '{ed_var}',
            url       = '{ed_info$base_url}',
            longitude = c({bb['xmin']}, {bb['xmax']}),
            latitude  = c({bb['ymin']}, {bb['ymax']}),
            time      = c('{date}', '{date}'))"))}

    if (all(c("lon", "lat") %in% colnames(nc$data))){
      d <- tibble(nc$data) %>%
        mutate(
          # round b/c of uneven intervals
          #   unique(tbl$lon) %>% sort() %>% diff() %>% unique() %>% as.character()
          #     0.0499954223632812 0.0500030517578125
          #   TODO: inform Maria/Joaquin about uneven intervals
          lon  = round(lon, 4),
          lat  = round(lat, 4),
          date = as.Date(time, "%Y-%m-%dT12:00:00Z"))
    } else if (all(c("longitude", "latitude") %in% colnames(nc$data))){
      d <- tibble(nc$data) %>%
        mutate(
          lon  = round(longitude, 4),
          lat  = round(latitude,  4),
          # lon  = longitude,
          # lat  = latitude,
          date = as.Date(time, "%Y-%m-%dT12:00:00Z"))
    } else {
      stop("Expected lon/lat or longitude/latitude in ERDDAP dataset.")
    }
    d_sp <- d |>
      select(lon, lat, !!ed_var)
    sp::coordinates(d_sp) <- ~ lon + lat
    # x0 <- x
    # sp::gridded(x) <- T
    # r <- raster::raster(x, layer = ed_var)
    if (ed_var == "chlorophyll"){
      g <- sp::points2grid(d_sp, tolerance = 0.0243902)
    } else {
      g <- sp::points2grid(d_sp)
    }
    # g <- sp::points2grid(x, tolerance = 1e-05)
    # g <- sp::points2grid(x), tolerance = 1e-05)
    # cx <- range(diff(sort(unique(d$lon))))
    # # dx <- range(diff(cx))
    # cy <- range(diff(sort(unique(d$lat))))
    # # dy <- range(diff(cy))
    # diff(c(unique(cx), unique(cy)))
    #
    # diff(cx), diff(cy)
    # tibble(
    #   cx = cx,
    #   cy = cy) |>
    #   expand(cx, cy) |>
    #   mutate(dif = )
    #
    # dy <- range(diff(sort(unique(d$lat))))
    # tol <- max(max(c(dx,dy)))
    # g <- try(sp::points2grid(x))
    # g <- sp::points2grid(x, tolerance = tol)
    r <- raster::raster(g)
    idx <- raster::cellFromXY(r, sp::coordinates(d_sp))
    r[idx] <- d_sp[[ed_var]]
    # plot(r)
    raster::crs(r) <- 4326
    r
  }

  tbl <- tibble(
    date = dates_get) %>%
    mutate(
      raster = map(date, get_ed_raster))

  if (!is.null(dir_tif) && nrow(tifs_match) > 0){
    if (verbose)
      message(glue("Binding {nrow(tbl)} grids from ERDDAP with {nrow(tbl_tifs)} grids from dir_tif."))

    tbl <- tbl %>%
      bind_rows(
        tbl_tifs) %>%
      arrange(date)
  }

  is_stack <- nrow(tbl) > 1

  if (verbose)
    message(glue("Reading, masking, naming and writing (if dir_tif) to grd."))

  if (!is_stack){
    if (verbose)
      message(glue("  from single raster"))

    grd <- tbl$raster[[1]]
    grd <- raster::mask(grd, sf::as_Spatial(ply))
    names(grd) <- glue("{ed_var}_{tbl$date[[1]]}")

    if (!is.null(dir_tif))
      raster::writeRaster(
        grd, glue("{dir_tif}/grd_{names(grd)}.tif"), overwrite = T)

  } else {
    if (verbose)
      message(glue("  from raster stack"))

    grd <- raster::stack(tbl$raster)
    grd <- raster::mask(grd, sf::as_Spatial(ply))
    names(grd) <- glue("{ed_var}_{tbl$date}")

    if (!is.null(dir_tif)){
      dir.create(dir_tif, showWarnings = F, recursive = T)

      # raster::writeRaster(
      #   grd, dir_grd, names(grd),
      #   bylayer=T, format='GTiff', overwrite = T)

      # TODO: switch from raster:: to terra::
      #librarian::shelf(terra)
      path_tif <- glue("{dir_tif}/grd_{names(grd)}.tif")
      if (verbose)
        message(glue("writing: {path_tif}"))
      terra::writeRaster(
        terra::rast(grd), path_tif,
        overwrite = T)
    }
  }

  # raster::plot(grd); plot(ply, add = T, col = scales::alpha("blue", 0.3))
  grd
}

#' Check if a URL is online
#'
#' Check if a URL is online by making a HEAD request. Returns TRUE if the URL is
#' reachable and the status code is between 200 and 399 (inclusive). Returns FALSE
#' otherwise.
#'
#' This function is useful for checking the availability of a server or
#' resource before attempting to access it. In particular, it is used by
#' `ed_info()` to check if the ERDDAP server is online before making requests
#' to it since the underlying `rerddap::info()` crashes R if `url` is offline.
#'
#' @param url link to check
#' @param timeout_ms timeout in milliseconds
#' @return list with online status, status code, and message
#' @importFrom crul HttpClient
#' @export
#' @concept read
#' @examples
#' check_url("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html")
check_url <- function(url, timeout_ms = 5000) {
  # use package crul already required by rerddap
  # url = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html"; timeout_ms = 5000

  # Create a new HTTP client with the given URL and timeout
  cli <- crul::HttpClient$new(
    url  = url,
    opts = list(
      timeout_ms = timeout_ms))

  # Try to make a HEAD request (lightweight, just gets headers)
  tryCatch({
    response <- cli$head()

    # Check status code
    status <- response$status_code

    # Return TRUE if status code is between 200-399 (success/redirect)
    return(list(
      online = status >= 200 && status < 400,
      status_code = status,
      message = paste("Server is online with status code:", status)
    ))
  }, error = function(e) {
    # Return FALSE if any error occurs (connection refused, timeout, etc.)
    return(list(
      online = FALSE,
      status_code = NULL,
      message = paste("Server is offline or unreachable:", e$message)
    ))
  })
}

#' Get ERDDAP dataset information
#'
#' Get ERDDAP dataset information.
#'
#' @param dataset `{region}_{frequency}` of dataset. Valid values (so far): "global_8day" or "global_monthly" (default).
#'
#' @return ERDDAP \code{\link[rerddap]{info}} object
#' @importFrom rerddap info
#' @importFrom fs path_ext_remove
#' @export
#' @concept read
#'
#' @examples
#' ed_info() # default: dataset = "global_monthly"
#' ed_info("global_8day")
ed_info <- function(dataset){
  #dataset = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41mday.html"

  # librarian:::is_valid_url
  is_valid_url <- function(string){
    any(grepl("(https?|ftp)://[^\\s/$.?#].[^\\s]*", string))
  }

  if (is_valid_url(dataset)){
    ed_url  <- dirname(dirname(dataset))
    dataset <- basename(dataset) |> fs::path_ext_remove()
  } else{
    ed_url = "https://coastwatch.pfeg.noaa.gov/erddap"
  }

  # test ERDDAP server is online
  status <- check_url(ed_url)
  if (!status$online)
    stop(glue("ERDDAP server is offline or unreachable: {status$message}"))

  rerddap::info(dataset, url = ed_url)
}

#' Get dimension values from ERDDAP dataset
#'
#' Given an ERDDAP dataset info object, return a vector of all available values in given dimension.
#'
#' @param ed_info ERDDAP info object on SeaScape dataset, as returned by \code{\link{ed_info}})
#' @param dim dimension to extract
#'
#' @return vector of values for given dimension
#'
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom dplyr pull
#' @export
#' @concept read
#'
#' @examples
#' ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")
#' ed_dim(ed, "LEV")
ed_dim <- function(ed, dim){
  # ed_info = ed_info("https://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_d749_a206_cd3a.html")
  # dim     = "time"

  ed_dataset = attr(ed, "datasetid")

  d_url <- glue("{ed$base_url}/griddap/{ed_dataset}.csvp?{dim}")
  d <- try(read_csv(d_url, show_col_types = F, progress = F))
  if ("try-error" %in% class(d))
    stop(glue("Problem fetching dimension {dim} from ERDDAP: {d_url}"))

  pull(d)
}

#' Get dimensions from ERDDAP dataset
#'
#' Given an ERDDAP dataset info object, return a character vector of all available dimensions.
#'
#' @param ed ERDDAP info object, as returned by \code{\link{ed_info}})
#'
#' @return character vector of dimension names
#'
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom dplyr pull
#' @export
#' @concept read
#'
#' @examples
#' ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")
#' ed_dims(ed)
ed_dims <- function(ed){
  vars <- ed_vars(ed)$variable_name
  dim_names <- names(ed$alldata) |>
    setdiff(c("NC_GLOBAL", vars))
  sapply(dim_names, ed_dim, ed = ed)
}

#' Get variables from ERDDAP dataset
#'
#' Given an ERDDAP dataset info object, return a character vector of all available
#' variables. Variables are values that vary along the dimensions of the ERDDAP
#' datasets (as returned by `ed_dims()`).
#'
#' @param ed ERDDAP info object, as returned by \code{\link{ed_info}})
#'
#' @return character vector of variable names
#'
#' @export
#' @concept read
#'
#' @examples
#' ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")
#' ed_vars(ed)
ed_vars <- function(ed){
  ed$variables
}

#' ERDDAP Extract
#'
#' Extract data from ERDDAP dataset by bounding box and/or polygon. This function
#' provides the following enhancements to `rerddap::griddap()`:
#' 1. Throttle request to `rerddap::griddap()` by chunks of time, so
#' the ERDDAP server does not become unresponsive with too big a request while
#' minimizing the number of requests.
#' 1. Mask the grids by Area of Interest (`aoi`) (using `terra::mask()`).
#' 1. Summarize grids over time by `aoi`, including sub-geometries, into a CSV (using `terra::zonal()`).
#' 1. Preserve intermediary NetCDF and/or GeoTIFF files for raster visualization.
#'
#' @param ed   ERDDAP info object, of class `rerddap::info` as returned by \code{\link{ed_info}})
#' @param var  variable to extract
#' @param sf_zones spatial feature object ([`sf`]) with zones to extract zonal
#'   statistics from ERDDAP gridded data. If `sf_zones` is left
#'   to the default `NULL` value, then the `bbox` is used.
#' @param fld_zones character vector of unique field name(s) in `sf_zones` to
#'   include in extracted zonal statistics
#' @param bbox bounding box (e.g.,
#' `c(xmin = -83.0, ymin = 27.3, xmax = -81.8, ymax= 28.5)`), used to extract
#' the grid (i.e., as `longitude` and `latitude`  arguments to `rerddap::griddap()`).
#' If `bbox` is left to the default `NULL` value, then the bounding box is derived from the bounding box of the
#' `sf_zones`.
#' @param zonal_csv output zonal statistics as CSV from `terra::zonal(rast_tif, aoi)`
#' @param zonal_fun function to summarize the data over time by `sf_zones`. Default:
#' @param rast_tif optional output as GeoTIFF masked to `sf_zones` from NetCDF
#' @param mask_tif mask the GeoTIFF by `sf_zones`. Default: TRUE
#' @param dir_nc optional output directory to keep NetCDF files written to disk
#' by `rerddap::griddap()`
#' @param keep_nc keep NetCDF files written to disk by `rerddap::griddap()`
#' server at a time. Default: 100,000
#' @param n_max_vals_per_req maximum number of values to request from ERDDAP
#' @param n_max_retries maximum number of retries to request data from ERDDAP
#' server
#' @param time_min minimum time to extract from ERDDAP dataset
#' @param time_max maximum time to extract from ERDDAP dataset
#' @param verbose display messages on status of function. Useful for debugging
#' or showing status while getting data from a wide range and/or big polygon.
#' Default: FALSE.
#' @param ...  arguments to pass along to `rerddap::griddap()`, such as to
#' filter the request by dimensions
#' @return data frame of the zonal summary of the raster (optionally also
#' written to zonal_csv)
#' @import sf rerddap
#' @importFrom terra crop ext ncell rast subset trim values
#' @importFrom readr read_csv
#' @importFrom fs file_exists file_delete file_move
#' @concept read
#' @export
#'
#' @examples
#' \dontrun{
#' ed <- ed_info("https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html")
#' (vars <- ed_vars(ed))
#' dims <- ed_dims(ed)
#' times = tail(dims$time, 10)
#' ed_extract(
#'   ed,
#'   "analysed_sst",
#'   bbox = c(xmin = -83.0, ymin = 27.3, xmax = -81.8, ymax= 28.5),
#'   time_min = min(times),
#'   time_max = max(times) )
#' }
ed_extract <- function(
    ed,
    var,
    sf_zones  = NULL,
    fld_zones = NULL,
    bbox      = NULL,
    zonal_csv = NULL,
    zonal_fun = "mean",
    rast_tif  = NULL,
    mask_tif  = TRUE,
    dir_nc    = NULL,
    keep_nc   = FALSE,
    n_max_vals_per_req = 100000,
    n_max_retries      = 3,
    time_min  = NULL,
    time_max  = NULL,
    verbose   = FALSE,
    ...){
  # TODO: append to tif if exists
  # TODO: +args dir_nc: infers keep_nc, otherwise use tmpdir
  # TODO: +args dir_tif: infers keep_tif, otherwise use tmpdir
  # TODO: +args fld_zones: report in csv by fld  (otherwise st_union)

  # DEBUG
  # devtools::document(); devtools::load_all()
  # ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")  # jplMURSST41 daily
  # ed <- ed_info("https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html")   # CoralTemp daily
  # var  = ed_vars(ed)$variable_name[1]  # "analysed_sst"
  # bbox = c(xmin = -83.0, ymin = 27.2, xmax = -82.3, ymax= 28.5)
  # sf_zones = tbeptools::tbsegshed
  # zonal_csv = here::here("data_tmp/tbep_sst.csv")
  # zonal_csv = "~/Github/tbep-tech/climate-change-indicators/data/sst/tbep_sst.csv"
  # dir_nc    = glue::glue("{fs::path_ext_remove(zonal_csv)}_nc")
  # rast_tif  = glue::glue("{fs::path_ext_remove(zonal_csv)}.tif")
  # ed_extract(ed, "analysed_sst", sf_zones, bbox, zonal_csv = zonal_csv, rast_tif = rast_tif, dir_nc = dir_nc, mask_tif = F)

  # check input arguments ----
  stopifnot(class(ed) == "info")
  if (!is.null(sf_zones))
    stopifnot("sf" %in% class(sf_zones))
  if (!is.null(fld_zones))
    stopifnot(fld_zones %in% names(sf_zones))
  if (!is.null(time_min))
    stopifnot("POSIXct" %in% class(time_min))
  if (!is.null(time_max))
    stopifnot("POSIXct" %in% class(time_max))
  if (!is.null(bbox))
    stopifnot(class(bbox) %in% c("bbox", "numeric"))
  stopifnot(zonal_fun %in% c("mean", "min", "max", "sum", "isNA", "notNA"))

  # listviewer::jsonedit(ed$alldata)
  vars <- ed_vars(ed)
  stopifnot(var %in% vars$variable_name)

  dims <- ed_dims(ed)
  dims_xyt <- c("longitude","latitude","time")
  # TODO: consider alternative names, eg lon / lat / date
  stopifnot(all(dims_xyt %in% names(dims)))

  if (is.null(dir_nc) & is.null(rast_tif))
    dir_nc <- tempdir()
  if (is.null(dir_nc) & !is.null(rast_tif))
    dir_nc <- paste0(fs::path_ext_remove(rast_tif), "_nc")
  if (!dir.exists(dir_nc))
    dir.create(dir_nc, showWarnings = F, recursive = T)

  time_min   <- ifelse(is.null(time_min), min(dims$time), time_min) |> as.POSIXct(tz = "UTC", origin="1970-01-01 00:00.00 UTC")
  time_max   <- ifelse(is.null(time_max), max(dims$time), time_max) |> as.POSIXct(tz = "UTC", origin="1970-01-01 00:00.00 UTC")
  times_todo <- dims$time[dims$time >= time_min & dims$time <= time_max]

  # browser() # DEBUG
  # filter and handle existing data
  # existing_data <- NULL
  # if (file.exists(zonal_csv)){
  #   d_z <- readr::read_csv(zonal_csv, show_col_types = F, progress = F)
  #
  #   # Keep existing data for later merging
  #   existing_data <- d_z
  #
  #   # Extract only new times that aren't in the existing CSV
  #   new_times <- setdiff(times_todo, d_z$time) |> as.POSIXct(tz = "UTC", origin="1970-01-01 00:00.00 UTC")
  #
  #   if (length(new_times) == 0){
  #     if (verbose)
  #       message(glue::glue("All times ({time_min} to {time_max}) are already present in {basename(zonal_csv)}, skipping ERDDAP fetch."))
  #     if (!keep_nc)
  #       unlink(dir_nc, recursive = T)
  #     return()
  #   }
  #
  #   # Update times_todo to only fetch new data
  #   times_todo <- new_times
  # }

  if (is.null(sf_zones) & is.null(bbox))
    stop("Please set argument into `ed_extract()` for `sf_zones`, `bbox` or both.")

  wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
  if (is.null(sf_zones))
    sf_zones <- bbox |>
      sf::st_bbox() |>
      sf::st_as_sfc() |>
      sf::st_as_sf(crs = wgs84)

  if (terra::crs(sf_zones, proj=T) != wgs84)
    sf_zones <- sf::st_transform(sf_zones, wgs84)

  if (is.null(bbox)){
    suppressMessages({
      sf::sf_use_s2(F)
      bbox <- sf_zones |>
        sf::st_union() |>
        sf::st_make_valid() |>
        sf::st_bbox()
      sf::sf_use_s2(T) }) }

  # TODO: check for irregular grid and use terra::rasterize to regularize grid values
  #       - [How to make RASTER from irregular point data without interpolation](https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation)
  # diff(dims$longitude) |> range() # 0.125  0.125
  # diff(dims$latitude) |> range()  # 0.1082 0.1250

  r_na <- expand.grid(
    longitude = dims$longitude,
    latitude  = dims$latitude,
    value     = NA) |>
    terra::rast(
      type = "xyz",
      crs  = wgs84)
  # TODO: check assumption of grid Geographic CRS

  if (terra::ext(r_na)[2] > 180){
    # a) either rotate raster to -180, 180
    #   r_na  <- terra::rotate(r_na, 180)
    # b) or shift vector to 0, 360
    sf_zones <- sf::st_shift_longitude(sf_zones) # xmin: -123.1401 ymin: 35.5 xmax: -121.1036 ymax: 37.88163
  }

  r_idx <- r_na
  terra::values(r_idx) <- 1:terra::ncell(r_idx)

  bb <- st_bbox(bbox)
  r_bb <- r_idx |>
    terra::crop(bb) |>
    terra::trim()
  n_r <- terra::ncell(r_bb)

  dims_other <- setdiff(names(dims), dims_xyt)
  n_dims_other <- ifelse(
    length(dims_other) > 0,
    sapply(dims[dims_other], length) |> prod(),
    1) # set to unity for multiplying with n_r

  # n_per_t: number of values per individual time slice
  n_per_t <- n_r * n_dims_other

  stopifnot(n_max_vals_per_req >= n_per_t)

  # n_t_per_req: number of time slices per request
  n_t_per_req <- n_max_vals_per_req %/% n_per_t
  n_t         <- length(times_todo)
  n_reqs      <- ceiling(n_t / n_t_per_req)

  if (verbose)
    message(glue("Downloading {n_reqs} requests, up to {n_t_per_req} time slices each"))

  i_req <- 1

  while (i_req <= n_reqs) {

    i_t_beg   <- (i_req - 1) * n_t_per_req + 1
    i_t_end   <- min(c(i_t_beg + n_t_per_req - 1, n_t))
    t_req     <- times_todo[c(i_t_beg, i_t_end)]

    t_req_str <- format_ISO8601(t_req, usetz="Z")
    # TODO: slice if not starting at i_t=1

    # TODO: skip slices already fetched based on tif / csv outputs
    # if (file.exists(zonal_csv)){
    #   d <- read_csv(zonal_csv, show_col_types=F)
    #
    #   if (all(as.POSIXct(t_req, tz = "UTC") %in% d$time)){
    #     if (verbose)
    #       message(glue("Skipping {i_t_beg}:{i_t_end} ({paste(as.Date(t_req), collapse = ' to ')}) of {n_t}, since already in csv ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
    #     # i_t_beg <- i_t_end + 1
    #     i_req <- i_req + 1
    #     next
    #   }
    # }
    if (verbose)
      message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))

    # delete 0 byte nc files in folder that cause rerddap::griddap() to fail
    # dir_nc <- "/share/github/noaa-onms/climate-dashboard-app/data/NOAA_DHW/GRNMS/2010_nc"
    ncs0 <- dplyr::tibble(
      nc   = list.files(dir_nc, ".*\\.nc$", full.names = T),
      size = file.size(nc)) |>
      dplyr::filter(size == 0) |>
      dplyr::pull(nc)
    unlink(ncs0)

    nc_retry <- T
    nc_n_try <- 1
    n_max_retries
    while (nc_retry){
      res <- try(rerddap::griddap(
        # datasetx = "doh"))
        datasetx  = attr(ed, "datasetid"),
        url       = ed$base_url,
        fields    = var,
        longitude = c(bbox[["xmin"]], bbox[["xmax"]]),
        latitude  = c(bbox[["ymin"]], bbox[["ymax"]]),
        time      = t_req_str,
        fmt       = "nc",
        store     = rerddap::disk(path = dir_nc) ) ) # ,
        # TODO: get subset based on args in (...), eg time subset
        # LEV = c(min(zs), max(zs))
        #!!!list(...)))

      if (inherits(res, "try-error")){
        err <-  attr(res, "condition")
        msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
        nc_n_try <- nc_n_try + 1
        if (nc_n_try > n_max_retries){
          stop(msg)
        } else {
          message(msg,"\nRETRYing...")
          Sys.sleep(1)
        }
      } else {
        nc_retry <- F
      }
    }

    i_req <- i_req + 1
  }
  # dir_nc <- "/share/github/noaa-onms/climate-dashboard-app/data/NOAA_DHW/CBNMS/2005_nc"
  # dir_nc <- "/share/github/noaa-onms/climate-dashboard-app/data/NOAA_DHW/FGBNMS/1999_nc"
  ncs <- dplyr::tibble(
    nc   = list.files(dir_nc, ".*\\.nc$", full.names = T),
    size = file.size(nc)) |>
    dplyr::filter(size > 0) |>
    dplyr::pull(nc)

  # TODO: combine with existing *.tif (esp. where missing {yr}_nc/*.nc dirs)
  # browser()
  # TODO: compare extents like terra::compareGeom(rast(ncs[2]), rast(ncs[3]))
  r <- terra::rast(ncs)

  # TODO: handle NetCDFs without time stamp
  stopifnot(all(class(terra::time(r)) %in% c("POSIXct","POSIXt")))

  idx <- dplyr::tibble(
    idx  = 1:terra::nlyr(r),
    time = terra::time(r)) |>
    dplyr::arrange(time) |>
    dplyr::filter(!duplicated(time)) |>
    dplyr::pull(idx)
  r <- terra::subset(r, idx)

  # TODO: handle projections outside wgs84
  stopifnot(terra::crs(r, proj=T) == wgs84)

  if (mask_tif)
    r <- terra::mask(r, sf_zones)

  # TODO: add trim at end of iterations
  # r <- terra::trim(r)

  # TBEP: bbox of tbeptools::tbshed with 10 km buffer and raster trimmed
  # ext(r) |> st_bbox() |>  round(1)
  # xmin  ymin  xmax  ymax
  # -83.0  27.2 -82.3  28.5

  # get times for layer names and trim to date if all equal hours-minutes-seconds
  #   leave full date-time for times(r) to referece exact ERDDAP time slice
  # v_hms <- hour(time(r)) + minute(time(r))/60 + second(time(r))/3600
  # is_hms_eq <- var(v_hms) == 0
  # if (is_hms_eq){
  #   lyr_times <- as.Date(terra::time(r)) |> as.character()
  # } else {
  #   lyr_times <- terra::time(r) |> str_replace_all(" ", "|") |> class() }
  #
  # lyrs <- glue("{var}_{lyr_times}")
  lyrs <- glue("{var}|{terra::time(r)}")
  if (length(dims_other) > 0 && !all(length(dims[dims_other]) == 1))
    stop(glue("Other dimensions not yet supported: {paste(dims_other, collapse = ',')}"))

  # TODO: include other dims (eg depth) in lyr names
  names(r) <- lyrs

  # i_lyr = 1
  # plot(r[[i_lyr]], main = names(r)[i_lyr])
  # terra::plet(
  #   r[[i_lyr]],
  #   col   = rev(RColorBrewer::brewer.pal(11, "Spectral")),
  #   tiles = leaflet::providers$CartoDB.DarkMatterNoLabels,
  #   main  = names(r)[i_lyr] |> stringr::str_replace("\\|","<br>")) |>
  #   leaflet::addProviderTiles(
  #     leaflet::providers$CartoDB.DarkMatterOnlyLabels,
  #     options = leaflet::providerTileOptions(
  #       opacity = 0.5))

  # browser() # DEBUG
  if (!is.null(rast_tif)){
    if (fs::file_exists(rast_tif)){
      r_tmp_tif <- tempfile(fileext = ".tif")
      r_tmp <- c(rast(rast_tif), r)                            # merge layers old and new
      r_tmp <- terra::subset(r_tmp, which(!duplicated(names(r_tmp)))) # rm duplicates
      terra::writeRaster(r_tmp, r_tmp_tif)
      fs::file_delete(rast_tif)
      fs::file_move(r_tmp_tif, rast_tif)
      rm(r); rm(r_tmp)
    } else {
      terra::writeRaster(r, rast_tif, overwrite = T, gdal=c("COMPRESS=DEFLATE"))
    }
    # fs::file_delete(ncs)
    r <- terra::rast(rast_tif)
  }


    d_r <- terra::zonal(
      x          = r,
      z          = terra::vect(
        sf_zones |>
          dplyr::select(dplyr::all_of(fld_zones))),
      fun         = zonal_fun,
      exact       = T,
      na.rm       = T,
      as.polygons = T) |>
      sf::st_as_sf() |>
      sf::st_drop_geometry() |>
      tidyr::pivot_longer(
        cols      = -dplyr::any_of(fld_zones),
        names_to  = "lyr",
        values_to = zonal_fun) |>
      mutate(
        time = str_replace(lyr, glue("{var}\\|(.*)"), "\\1") |>
          readr::parse_datetime())

    # Merge with existing data if available
    # if (!is.null(existing_data)) {
    #   # Remove any duplicate entries (same time and zone) from existing data
    #   # that match the new data to ensure any duplicates are overwritten
    #   if (!is.null(fld_zones) && length(fld_zones) > 0) {
    #     # If we have zone fields, remove duplicates by time and zone fields
    #     existing_filtered <- dplyr::anti_join(
    #       existing_data,
    #       d_r,
    #       by = c("time", fld_zones))
    #   } else {
    #     # Otherwise just use time
    #     existing_filtered <- dplyr::anti_join(
    #       existing_data,
    #       d_r,
    #       by = "time")
    #   }
    #
    #   # Combine filtered existing data with new data
    #   d_r <- dplyr::bind_rows(existing_filtered, d_r) |>
    #     dplyr::arrange(time)
    #
    #   if (verbose)
    #     message(glue::glue("Merged {nrow(d_r) - nrow(existing_filtered)} new rows with {nrow(existing_filtered)} existing rows in {basename(zonal_csv)}"))
    # }
  if (!is.null(zonal_csv))
    write_csv(d_r, zonal_csv)

  if (!keep_nc)
    unlink(dir_nc, recursive = T)

  return(d_r)
}

