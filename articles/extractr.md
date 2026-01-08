# extractr

Load packages and set up the environment.

``` r
library(extractr)
```

``` r
suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(mapview)
  library(readr)
  library(sf)
  library(terra)
})
options(readr.show_col_types = F)
```

## Get area of interest (AoI)

Load area of interest (AoI) from this
[sanctuaries.geojson](https://github.com/noaa-onms/onmsR/blob/master/data-raw/sanctuaries.geojson)
for the Florida Keys National Marine Sanctuary (FKNMS).

``` r
aoi_geo <- "https://raw.githubusercontent.com/noaa-onms/onmsR/master/data-raw/sanctuaries.geojson"

aoi <- read_sf(aoi_geo) |> 
  filter(nms == "FKNMS")
(bb <- st_bbox(aoi))
#>      xmin      ymin      xmax      ymax 
#> -83.14989  24.30041 -80.06647  25.65046

mapView(aoi)
```

## Dataset: sea surface temperature

### Get info

ERDDAP dataset:

- [ERDDAP - Sea Surface Temperature, NOAA Coral Reef Watch Daily Global
  5km Satellite SST (CoralTemp), 1985-present, Daily - Data Access
  Form](https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html)

``` r
ed_url  <- "https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html"
(ed <- ed_info(ed_url))
#> <ERDDAP info> noaacrwsstDaily 
#>  Base URL: https://coastwatch.noaa.gov/erddap 
#>  Dataset Type: griddap 
#>  Dimensions (range):  
#>      time: (1985-01-01T12:00:00Z, 2026-01-07T12:00:00Z) 
#>      latitude: (-89.975, 89.975) 
#>      longitude: (-179.975, 179.975) 
#>  Variables:  
#>      analysed_sst: 
#>          Units: degree_C 
#>      sea_ice_fraction: 
#>          Units: 1
```

### Get dimensions

``` r
var <- "analysed_sst"

dims <- ed_dims(ed)
names(dims)
#> [1] "time"      "latitude"  "longitude"

# let's limit to the most recent 10 times
(times <- tail(dims[["time"]], 10))
#>  [1] "2025-12-29 12:00:00 UTC" "2025-12-30 12:00:00 UTC"
#>  [3] "2025-12-31 12:00:00 UTC" "2026-01-01 12:00:00 UTC"
#>  [5] "2026-01-02 12:00:00 UTC" "2026-01-03 12:00:00 UTC"
#>  [7] "2026-01-04 12:00:00 UTC" "2026-01-05 12:00:00 UTC"
#>  [9] "2026-01-06 12:00:00 UTC" "2026-01-07 12:00:00 UTC"
```

### Extract dataset from AoI

``` r
d_csv <- here("data_tmp/sst_timeseries.csv")
r_tif <- here("data_tmp/sst_raster.tif")

(d <- ed_extract(
  ed        = ed,
  var       = var,
  sf_zones  = aoi,
  fld_zones = "nms",
  zonal_fun = "mean",
  zonal_csv = d_csv,
  rast_tif  = r_tif,
  time_min  = times[1],
  verbose   = TRUE))
#> Downloading 1 requests, up to 59 time slices each
#> Called from: ed_extract(ed = ed, var = var, sf_zones = aoi, fld_zones = "nms", 
#>     zonal_fun = "mean", zonal_csv = d_csv, rast_tif = r_tif, 
#>     time_min = times[1], verbose = TRUE)
#> debug: while (i_req <= n_reqs) {
#>     i_t_beg <- (i_req - 1) * n_t_per_req + 1
#>     i_t_end <- min(c(i_t_beg + n_t_per_req - 1, n_t))
#>     t_req <- times_todo[c(i_t_beg, i_t_end)]
#>     t_req_str <- format_ISO8601(t_req, usetz = "Z")
#>     if (verbose) 
#>         message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#>     ncs0 <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>         ".*\\.nc$", full.names = T), size = file.size(nc)), size == 
#>         0), nc)
#>     unlink(ncs0)
#>     nc_retry <- T
#>     nc_n_try <- 1
#>     n_max_retries
#>     while (nc_retry) {
#>         res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>             url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>                 bbox[["xmax"]]), latitude = c(bbox[["ymin"]], 
#>                 bbox[["ymax"]]), time = t_req_str, fmt = "nc", 
#>             store = rerddap::disk(path = dir_nc)))
#>         if (inherits(res, "try-error")) {
#>             err <- attr(res, "condition")
#>             msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>             nc_n_try <- nc_n_try + 1
#>             if (nc_n_try > n_max_retries) {
#>                 stop(msg)
#>             }
#>             else {
#>                 message(msg, "\nRETRYing...")
#>                 Sys.sleep(1)
#>             }
#>         }
#>         else {
#>             nc_retry <- F
#>         }
#>     }
#>     i_req <- i_req + 1
#> }
#> debug: i_t_beg <- (i_req - 1) * n_t_per_req + 1
#> debug: i_t_end <- min(c(i_t_beg + n_t_per_req - 1, n_t))
#> debug: t_req <- times_todo[c(i_t_beg, i_t_end)]
#> debug: t_req_str <- format_ISO8601(t_req, usetz = "Z")
#> debug: if (verbose) message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#> debug: message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#> Fetching request 1 of 1 (2025-12-29 to 2026-01-07) ~ 19:29:31 UTC
#> debug: ncs0 <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>     ".*\\.nc$", full.names = T), size = file.size(nc)), size == 
#>     0), nc)
#> debug: unlink(ncs0)
#> debug: nc_retry <- T
#> debug: nc_n_try <- 1
#> debug: n_max_retries
#> debug: while (nc_retry) {
#>     res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>         url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>             bbox[["xmax"]]), latitude = c(bbox[["ymin"]], bbox[["ymax"]]), 
#>         time = t_req_str, fmt = "nc", store = rerddap::disk(path = dir_nc)))
#>     if (inherits(res, "try-error")) {
#>         err <- attr(res, "condition")
#>         msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>         nc_n_try <- nc_n_try + 1
#>         if (nc_n_try > n_max_retries) {
#>             stop(msg)
#>         }
#>         else {
#>             message(msg, "\nRETRYing...")
#>             Sys.sleep(1)
#>         }
#>     }
#>     else {
#>         nc_retry <- F
#>     }
#> }
#> debug: res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>     url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>         bbox[["xmax"]]), latitude = c(bbox[["ymin"]], bbox[["ymax"]]), 
#>     time = t_req_str, fmt = "nc", store = rerddap::disk(path = dir_nc)))
#> debug: if (inherits(res, "try-error")) {
#>     err <- attr(res, "condition")
#>     msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>     nc_n_try <- nc_n_try + 1
#>     if (nc_n_try > n_max_retries) {
#>         stop(msg)
#>     }
#>     else {
#>         message(msg, "\nRETRYing...")
#>         Sys.sleep(1)
#>     }
#> } else {
#>     nc_retry <- F
#> }
#> debug: nc_retry <- F
#> debug: (while) nc_retry
#> debug: i_req <- i_req + 1
#> debug: (while) i_req <= n_reqs
#> debug: ncs <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>     ".*\\.nc$", full.names = T), size = file.size(nc)), size > 
#>     0), nc)
#> debug: r <- terra::rast(ncs)
#> debug: stopifnot(all(class(terra::time(r)) %in% c("POSIXct", "POSIXt")))
#> debug: idx <- dplyr::pull(dplyr::filter(dplyr::arrange(dplyr::tibble(idx = 1:terra::nlyr(r), 
#>     time = terra::time(r)), time), !duplicated(time)), idx)
#> debug: r <- terra::subset(r, idx)
#> debug: stopifnot(terra::crs(r, proj = T) == wgs84)
#> debug: if (mask_tif) r <- terra::mask(r, sf_zones)
#> debug: r <- terra::mask(r, sf_zones)
#> debug: lyrs <- glue("{var}|{terra::time(r)}")
#> debug: if (length(dims_other) > 0 && !all(length(dims[dims_other]) == 
#>     1)) stop(glue("Other dimensions not yet supported: {paste(dims_other, collapse = ',')}"))
#> debug: names(r) <- lyrs
#> debug: if (!is.null(rast_tif)) {
#>     if (fs::file_exists(rast_tif)) {
#>         r_tmp_tif <- tempfile(fileext = ".tif")
#>         r_tmp <- c(rast(rast_tif), r)
#>         r_tmp <- terra::subset(r_tmp, which(!duplicated(names(r_tmp))))
#>         terra::writeRaster(r_tmp, r_tmp_tif)
#>         fs::file_delete(rast_tif)
#>         fs::file_move(r_tmp_tif, rast_tif)
#>         rm(r)
#>         rm(r_tmp)
#>     }
#>     else {
#>         terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#>     }
#>     r <- terra::rast(rast_tif)
#> }
#> debug: if (fs::file_exists(rast_tif)) {
#>     r_tmp_tif <- tempfile(fileext = ".tif")
#>     r_tmp <- c(rast(rast_tif), r)
#>     r_tmp <- terra::subset(r_tmp, which(!duplicated(names(r_tmp))))
#>     terra::writeRaster(r_tmp, r_tmp_tif)
#>     fs::file_delete(rast_tif)
#>     fs::file_move(r_tmp_tif, rast_tif)
#>     rm(r)
#>     rm(r_tmp)
#> } else {
#>     terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#> }
#> debug: terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#> debug: r <- terra::rast(rast_tif)
#> debug: d_r <- mutate(tidyr::pivot_longer(sf::st_drop_geometry(sf::st_as_sf(terra::zonal(x = r, 
#>     z = terra::vect(dplyr::select(sf_zones, dplyr::all_of(fld_zones))), 
#>     fun = zonal_fun, exact = T, na.rm = T, as.polygons = T))), 
#>     cols = -dplyr::any_of(fld_zones), names_to = "lyr", values_to = zonal_fun), 
#>     time = readr::parse_datetime(str_replace(lyr, glue("{var}\\|(.*)"), 
#>         "\\1")))
#> debug: if (!is.null(zonal_csv)) write_csv(d_r, zonal_csv)
#> debug: write_csv(d_r, zonal_csv)
#> debug: if (!keep_nc) unlink(dir_nc, recursive = T)
#> debug: unlink(dir_nc, recursive = T)
#> debug: return(d_r)
#> # A tibble: 10 × 4
#>    nms   lyr                               mean time               
#>    <chr> <chr>                            <dbl> <dttm>             
#>  1 FKNMS analysed_sst|2025-12-29 12:00:00  24.2 2025-12-29 12:00:00
#>  2 FKNMS analysed_sst|2025-12-30 12:00:00  24.2 2025-12-30 12:00:00
#>  3 FKNMS analysed_sst|2025-12-31 12:00:00  23.6 2025-12-31 12:00:00
#>  4 FKNMS analysed_sst|2026-01-01 12:00:00  22.9 2026-01-01 12:00:00
#>  5 FKNMS analysed_sst|2026-01-02 12:00:00  22.4 2026-01-02 12:00:00
#>  6 FKNMS analysed_sst|2026-01-03 12:00:00  22.4 2026-01-03 12:00:00
#>  7 FKNMS analysed_sst|2026-01-04 12:00:00  22.6 2026-01-04 12:00:00
#>  8 FKNMS analysed_sst|2026-01-05 12:00:00  22.9 2026-01-05 12:00:00
#>  9 FKNMS analysed_sst|2026-01-06 12:00:00  23.1 2026-01-06 12:00:00
#> 10 FKNMS analysed_sst|2026-01-07 12:00:00  23.3 2026-01-07 12:00:00
```

### Plot time series

``` r
d <- read_csv(d_csv)
head(d)
#> # A tibble: 6 × 4
#>   nms   lyr                               mean time               
#>   <chr> <chr>                            <dbl> <dttm>             
#> 1 FKNMS analysed_sst|2025-12-29 12:00:00  24.2 2025-12-29 12:00:00
#> 2 FKNMS analysed_sst|2025-12-30 12:00:00  24.2 2025-12-30 12:00:00
#> 3 FKNMS analysed_sst|2025-12-31 12:00:00  23.6 2025-12-31 12:00:00
#> 4 FKNMS analysed_sst|2026-01-01 12:00:00  22.9 2026-01-01 12:00:00
#> 5 FKNMS analysed_sst|2026-01-02 12:00:00  22.4 2026-01-02 12:00:00
#> 6 FKNMS analysed_sst|2026-01-03 12:00:00  22.4 2026-01-03 12:00:00

plot_ts(d, label = "Surface Temperature (ºC)")
```

### Map raster

``` r
r <- rast(r_tif)
names(r)
#>  [1] "analysed_sst|2025-12-29 12:00:00" "analysed_sst|2025-12-30 12:00:00"
#>  [3] "analysed_sst|2025-12-31 12:00:00" "analysed_sst|2026-01-01 12:00:00"
#>  [5] "analysed_sst|2026-01-02 12:00:00" "analysed_sst|2026-01-03 12:00:00"
#>  [7] "analysed_sst|2026-01-04 12:00:00" "analysed_sst|2026-01-05 12:00:00"
#>  [9] "analysed_sst|2026-01-06 12:00:00" "analysed_sst|2026-01-07 12:00:00"

lyr <- names(r)[1]
plet(r[lyr], tiles = "Esri.OceanBasemap")
#> Warning in colors(.): Some values were outside the color scale and will be
#> treated as NA
```

## Dataset: sea surface salinity

### Get info

ERDDAP dataset:

- [ERDDAP - Sea Surface Salinity, Miras SMOS, Near Real-Time, Global
  0.25°, 2010-present, 3 Day Composite - Data Access
  Form](https://coastwatch.noaa.gov/erddap/griddap/noaacwSMOSsss3day.html)

``` r
ed_url  <- "https://coastwatch.noaa.gov/erddap/griddap/noaacwSMOSsss3day.html"
ed <- ed_info(ed_url)
ed
#> <ERDDAP info> noaacwSMOSsss3day 
#>  Base URL: https://coastwatch.noaa.gov/erddap 
#>  Dataset Type: griddap 
#>  Dimensions (range):  
#>      time: (2010-06-03T12:00:00Z, 2026-01-01T12:00:00Z) 
#>      altitude: (0.0, 0.0) 
#>      latitude: (-89.875, 89.875) 
#>      longitude: (-179.875, 179.875) 
#>  Variables:  
#>      sss: 
#>          Units: PSU 
#>      sss_dif: 
#>          Units: PSU
```

### Get dimensions

``` r
var <- "sss"

dims <- ed_dims(ed)
names(dims)
#> [1] "time"      "altitude"  "latitude"  "longitude"

# let's limit to the most recent 10 times
(times <- tail(dims[["time"]], 10))
#>  [1] "2025-12-06 12:00:00 UTC" "2025-12-09 12:00:00 UTC"
#>  [3] "2025-12-12 12:00:00 UTC" "2025-12-15 12:00:00 UTC"
#>  [5] "2025-12-18 12:00:00 UTC" "2025-12-21 12:00:00 UTC"
#>  [7] "2025-12-24 12:00:00 UTC" "2025-12-27 12:00:00 UTC"
#>  [9] "2025-12-30 12:00:00 UTC" "2026-01-01 12:00:00 UTC"
```

### Extract dataset from AoI

``` r
d_csv <- here("data_tmp/sss_timeseries.csv")
r_tif <- here("data_tmp/sss_raster.tif")

(d <- ed_extract(
  ed        = ed,
  var       = var,
  sf_zones  = aoi,
  fld_zones = "nms",
  zonal_fun = "mean",
  zonal_csv = d_csv,
  rast_tif  = r_tif,
  time_min  = times[1],
  verbose   = TRUE))
#> Downloading 1 requests, up to 1282 time slices each
#> Called from: ed_extract(ed = ed, var = var, sf_zones = aoi, fld_zones = "nms", 
#>     zonal_fun = "mean", zonal_csv = d_csv, rast_tif = r_tif, 
#>     time_min = times[1], verbose = TRUE)
#> debug: while (i_req <= n_reqs) {
#>     i_t_beg <- (i_req - 1) * n_t_per_req + 1
#>     i_t_end <- min(c(i_t_beg + n_t_per_req - 1, n_t))
#>     t_req <- times_todo[c(i_t_beg, i_t_end)]
#>     t_req_str <- format_ISO8601(t_req, usetz = "Z")
#>     if (verbose) 
#>         message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#>     ncs0 <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>         ".*\\.nc$", full.names = T), size = file.size(nc)), size == 
#>         0), nc)
#>     unlink(ncs0)
#>     nc_retry <- T
#>     nc_n_try <- 1
#>     n_max_retries
#>     while (nc_retry) {
#>         res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>             url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>                 bbox[["xmax"]]), latitude = c(bbox[["ymin"]], 
#>                 bbox[["ymax"]]), time = t_req_str, fmt = "nc", 
#>             store = rerddap::disk(path = dir_nc)))
#>         if (inherits(res, "try-error")) {
#>             err <- attr(res, "condition")
#>             msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>             nc_n_try <- nc_n_try + 1
#>             if (nc_n_try > n_max_retries) {
#>                 stop(msg)
#>             }
#>             else {
#>                 message(msg, "\nRETRYing...")
#>                 Sys.sleep(1)
#>             }
#>         }
#>         else {
#>             nc_retry <- F
#>         }
#>     }
#>     i_req <- i_req + 1
#> }
#> debug: i_t_beg <- (i_req - 1) * n_t_per_req + 1
#> debug: i_t_end <- min(c(i_t_beg + n_t_per_req - 1, n_t))
#> debug: t_req <- times_todo[c(i_t_beg, i_t_end)]
#> debug: t_req_str <- format_ISO8601(t_req, usetz = "Z")
#> debug: if (verbose) message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#> debug: message(glue("Fetching request {i_req} of {n_reqs} ({paste(as.Date(t_req), collapse = ' to ')}) ~ {format(Sys.time(), '%H:%M:%S %Z')}"))
#> Fetching request 1 of 1 (2025-12-06 to 2026-01-01) ~ 19:29:40 UTC
#> debug: ncs0 <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>     ".*\\.nc$", full.names = T), size = file.size(nc)), size == 
#>     0), nc)
#> debug: unlink(ncs0)
#> debug: nc_retry <- T
#> debug: nc_n_try <- 1
#> debug: n_max_retries
#> debug: while (nc_retry) {
#>     res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>         url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>             bbox[["xmax"]]), latitude = c(bbox[["ymin"]], bbox[["ymax"]]), 
#>         time = t_req_str, fmt = "nc", store = rerddap::disk(path = dir_nc)))
#>     if (inherits(res, "try-error")) {
#>         err <- attr(res, "condition")
#>         msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>         nc_n_try <- nc_n_try + 1
#>         if (nc_n_try > n_max_retries) {
#>             stop(msg)
#>         }
#>         else {
#>             message(msg, "\nRETRYing...")
#>             Sys.sleep(1)
#>         }
#>     }
#>     else {
#>         nc_retry <- F
#>     }
#> }
#> debug: res <- try(rerddap::griddap(datasetx = attr(ed, "datasetid"), 
#>     url = ed$base_url, fields = var, longitude = c(bbox[["xmin"]], 
#>         bbox[["xmax"]]), latitude = c(bbox[["ymin"]], bbox[["ymax"]]), 
#>     time = t_req_str, fmt = "nc", store = rerddap::disk(path = dir_nc)))
#> debug: if (inherits(res, "try-error")) {
#>     err <- attr(res, "condition")
#>     msg <- glue::glue("  ERROR in calling {err$call}:\n {err$message}")
#>     nc_n_try <- nc_n_try + 1
#>     if (nc_n_try > n_max_retries) {
#>         stop(msg)
#>     }
#>     else {
#>         message(msg, "\nRETRYing...")
#>         Sys.sleep(1)
#>     }
#> } else {
#>     nc_retry <- F
#> }
#> debug: nc_retry <- F
#> debug: (while) nc_retry
#> debug: i_req <- i_req + 1
#> debug: (while) i_req <= n_reqs
#> debug: ncs <- dplyr::pull(dplyr::filter(dplyr::tibble(nc = list.files(dir_nc, 
#>     ".*\\.nc$", full.names = T), size = file.size(nc)), size > 
#>     0), nc)
#> debug: r <- terra::rast(ncs)
#> debug: stopifnot(all(class(terra::time(r)) %in% c("POSIXct", "POSIXt")))
#> debug: idx <- dplyr::pull(dplyr::filter(dplyr::arrange(dplyr::tibble(idx = 1:terra::nlyr(r), 
#>     time = terra::time(r)), time), !duplicated(time)), idx)
#> debug: r <- terra::subset(r, idx)
#> debug: stopifnot(terra::crs(r, proj = T) == wgs84)
#> debug: if (mask_tif) r <- terra::mask(r, sf_zones)
#> debug: r <- terra::mask(r, sf_zones)
#> debug: lyrs <- glue("{var}|{terra::time(r)}")
#> debug: if (length(dims_other) > 0 && !all(length(dims[dims_other]) == 
#>     1)) stop(glue("Other dimensions not yet supported: {paste(dims_other, collapse = ',')}"))
#> debug: names(r) <- lyrs
#> debug: if (!is.null(rast_tif)) {
#>     if (fs::file_exists(rast_tif)) {
#>         r_tmp_tif <- tempfile(fileext = ".tif")
#>         r_tmp <- c(rast(rast_tif), r)
#>         r_tmp <- terra::subset(r_tmp, which(!duplicated(names(r_tmp))))
#>         terra::writeRaster(r_tmp, r_tmp_tif)
#>         fs::file_delete(rast_tif)
#>         fs::file_move(r_tmp_tif, rast_tif)
#>         rm(r)
#>         rm(r_tmp)
#>     }
#>     else {
#>         terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#>     }
#>     r <- terra::rast(rast_tif)
#> }
#> debug: if (fs::file_exists(rast_tif)) {
#>     r_tmp_tif <- tempfile(fileext = ".tif")
#>     r_tmp <- c(rast(rast_tif), r)
#>     r_tmp <- terra::subset(r_tmp, which(!duplicated(names(r_tmp))))
#>     terra::writeRaster(r_tmp, r_tmp_tif)
#>     fs::file_delete(rast_tif)
#>     fs::file_move(r_tmp_tif, rast_tif)
#>     rm(r)
#>     rm(r_tmp)
#> } else {
#>     terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#> }
#> debug: terra::writeRaster(r, rast_tif, overwrite = T, gdal = c("COMPRESS=DEFLATE"))
#> debug: r <- terra::rast(rast_tif)
#> debug: d_r <- mutate(tidyr::pivot_longer(sf::st_drop_geometry(sf::st_as_sf(terra::zonal(x = r, 
#>     z = terra::vect(dplyr::select(sf_zones, dplyr::all_of(fld_zones))), 
#>     fun = zonal_fun, exact = T, na.rm = T, as.polygons = T))), 
#>     cols = -dplyr::any_of(fld_zones), names_to = "lyr", values_to = zonal_fun), 
#>     time = readr::parse_datetime(str_replace(lyr, glue("{var}\\|(.*)"), 
#>         "\\1")))
#> debug: if (!is.null(zonal_csv)) write_csv(d_r, zonal_csv)
#> debug: write_csv(d_r, zonal_csv)
#> debug: if (!keep_nc) unlink(dir_nc, recursive = T)
#> debug: unlink(dir_nc, recursive = T)
#> debug: return(d_r)
#> # A tibble: 10 × 4
#>    nms   lyr                      mean time               
#>    <chr> <chr>                   <dbl> <dttm>             
#>  1 FKNMS sss|2025-12-06 12:00:00  32.1 2025-12-06 12:00:00
#>  2 FKNMS sss|2025-12-09 12:00:00  36.7 2025-12-09 12:00:00
#>  3 FKNMS sss|2025-12-12 12:00:00  37.4 2025-12-12 12:00:00
#>  4 FKNMS sss|2025-12-15 12:00:00  36.2 2025-12-15 12:00:00
#>  5 FKNMS sss|2025-12-18 12:00:00  36.6 2025-12-18 12:00:00
#>  6 FKNMS sss|2025-12-21 12:00:00  36.1 2025-12-21 12:00:00
#>  7 FKNMS sss|2025-12-24 12:00:00  33.8 2025-12-24 12:00:00
#>  8 FKNMS sss|2025-12-27 12:00:00  36.8 2025-12-27 12:00:00
#>  9 FKNMS sss|2025-12-30 12:00:00  34.6 2025-12-30 12:00:00
#> 10 FKNMS sss|2026-01-01 12:00:00  37.6 2026-01-01 12:00:00
```

### Plot time series

``` r
d <- read_csv(d_csv)
head(d)
#> # A tibble: 6 × 4
#>   nms   lyr                      mean time               
#>   <chr> <chr>                   <dbl> <dttm>             
#> 1 FKNMS sss|2025-12-06 12:00:00  32.1 2025-12-06 12:00:00
#> 2 FKNMS sss|2025-12-09 12:00:00  36.7 2025-12-09 12:00:00
#> 3 FKNMS sss|2025-12-12 12:00:00  37.4 2025-12-12 12:00:00
#> 4 FKNMS sss|2025-12-15 12:00:00  36.2 2025-12-15 12:00:00
#> 5 FKNMS sss|2025-12-18 12:00:00  36.6 2025-12-18 12:00:00
#> 6 FKNMS sss|2025-12-21 12:00:00  36.1 2025-12-21 12:00:00

plot_ts(d, label = "Surface Salinity (PSU)")
```

### Map raster

``` r
r <- rast(r_tif)
names(r)
#>  [1] "sss|2025-12-06 12:00:00" "sss|2025-12-09 12:00:00"
#>  [3] "sss|2025-12-12 12:00:00" "sss|2025-12-15 12:00:00"
#>  [5] "sss|2025-12-18 12:00:00" "sss|2025-12-21 12:00:00"
#>  [7] "sss|2025-12-24 12:00:00" "sss|2025-12-27 12:00:00"
#>  [9] "sss|2025-12-30 12:00:00" "sss|2026-01-01 12:00:00"

lyr <- names(r)[1]
plet(r[lyr], tiles = "Esri.OceanBasemap")
#> Warning in colors(.): Some values were outside the color scale and will be
#> treated as NA
```
