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
var <- "analysed_sst"

(ed <- ed_info(ed_url))
#> <ERDDAP(TM) info> noaacrwsstDaily 
#>  Base URL: https://coastwatch.noaa.gov/erddap 
#>  Dataset Type: griddap 
#>  Dimensions (range):  
#>      time: (1985-01-01T12:00:00Z, 2026-02-28T12:00:00Z) 
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
tryCatch({
  dims <- ed_dims(ed)
  names(dims)
  times <- tail(dims[["time"]], 10)
  print(times)
}, error = function(e) {
  message("Could not fetch ERDDAP time dimension: ", e$message)
  times <- NULL
})
#>  [1] "2026-02-19 12:00:00 UTC" "2026-02-20 12:00:00 UTC"
#>  [3] "2026-02-21 12:00:00 UTC" "2026-02-22 12:00:00 UTC"
#>  [5] "2026-02-23 12:00:00 UTC" "2026-02-24 12:00:00 UTC"
#>  [7] "2026-02-25 12:00:00 UTC" "2026-02-26 12:00:00 UTC"
#>  [9] "2026-02-27 12:00:00 UTC" "2026-02-28 12:00:00 UTC"
```

``` r
# let's limit to the most recent 10 times
(times <- tail(dims[["time"]], 10))
#>  [1] "2026-02-19 12:00:00 UTC" "2026-02-20 12:00:00 UTC"
#>  [3] "2026-02-21 12:00:00 UTC" "2026-02-22 12:00:00 UTC"
#>  [5] "2026-02-23 12:00:00 UTC" "2026-02-24 12:00:00 UTC"
#>  [7] "2026-02-25 12:00:00 UTC" "2026-02-26 12:00:00 UTC"
#>  [9] "2026-02-27 12:00:00 UTC" "2026-02-28 12:00:00 UTC"
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
#> Fetching request 1 of 1 (2026-02-19 to 2026-02-28) ~ 17:13:25 UTC
#> # A tibble: 10 × 4
#>    nms   lyr                               mean time               
#>    <chr> <chr>                            <dbl> <dttm>             
#>  1 FKNMS analysed_sst|2026-02-19 12:00:00  22.5 2026-02-19 12:00:00
#>  2 FKNMS analysed_sst|2026-02-20 12:00:00  23.1 2026-02-20 12:00:00
#>  3 FKNMS analysed_sst|2026-02-21 12:00:00  23.4 2026-02-21 12:00:00
#>  4 FKNMS analysed_sst|2026-02-22 12:00:00  23.5 2026-02-22 12:00:00
#>  5 FKNMS analysed_sst|2026-02-23 12:00:00  22.9 2026-02-23 12:00:00
#>  6 FKNMS analysed_sst|2026-02-24 12:00:00  21.6 2026-02-24 12:00:00
#>  7 FKNMS analysed_sst|2026-02-25 12:00:00  21.0 2026-02-25 12:00:00
#>  8 FKNMS analysed_sst|2026-02-26 12:00:00  20.9 2026-02-26 12:00:00
#>  9 FKNMS analysed_sst|2026-02-27 12:00:00  21.9 2026-02-27 12:00:00
#> 10 FKNMS analysed_sst|2026-02-28 12:00:00  22.9 2026-02-28 12:00:00
```

### Plot time series

``` r
d <- read_csv(d_csv)
head(d)
#> # A tibble: 6 × 4
#>   nms   lyr                               mean time               
#>   <chr> <chr>                            <dbl> <dttm>             
#> 1 FKNMS analysed_sst|2026-02-19 12:00:00  22.5 2026-02-19 12:00:00
#> 2 FKNMS analysed_sst|2026-02-20 12:00:00  23.1 2026-02-20 12:00:00
#> 3 FKNMS analysed_sst|2026-02-21 12:00:00  23.4 2026-02-21 12:00:00
#> 4 FKNMS analysed_sst|2026-02-22 12:00:00  23.5 2026-02-22 12:00:00
#> 5 FKNMS analysed_sst|2026-02-23 12:00:00  22.9 2026-02-23 12:00:00
#> 6 FKNMS analysed_sst|2026-02-24 12:00:00  21.6 2026-02-24 12:00:00

plot_ts(d, label = "Surface Temperature (ºC)")
```

### Map raster

``` r
r <- rast(r_tif)
names(r)
#>  [1] "analysed_sst|2026-02-19 12:00:00" "analysed_sst|2026-02-20 12:00:00"
#>  [3] "analysed_sst|2026-02-21 12:00:00" "analysed_sst|2026-02-22 12:00:00"
#>  [5] "analysed_sst|2026-02-23 12:00:00" "analysed_sst|2026-02-24 12:00:00"
#>  [7] "analysed_sst|2026-02-25 12:00:00" "analysed_sst|2026-02-26 12:00:00"
#>  [9] "analysed_sst|2026-02-27 12:00:00" "analysed_sst|2026-02-28 12:00:00"

lyr <- names(r)[1]
plet(r[lyr], tiles = "Esri.OceanBasemap")
```

## Dataset: sea surface salinity

### Get info

ERDDAP dataset:

- [ERDDAP - Sea Surface Salinity, Miras SMOS, Near Real-Time, Global
  0.25°, 2010-present, 3 Day Composite - Data Access
  Form](https://coastwatch.noaa.gov/erddap/griddap/noaacwSMOSsss3day.html)

``` r
ed_url  <- "https://coastwatch.noaa.gov/erddap/griddap/noaacwSMOSsss3day.html"
var <- "sss"

(ed <- ed_info(ed_url))
#> <ERDDAP(TM) info> noaacwSMOSsss3day 
#>  Base URL: https://coastwatch.noaa.gov/erddap 
#>  Dataset Type: griddap 
#>  Dimensions (range):  
#>      time: (2010-06-03T12:00:00Z, 2026-02-24T12:00:00Z) 
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
tryCatch({
  dims <- ed_dims(ed)
  names(dims)
  times <- tail(dims[["time"]], 10)
  print(times)
}, error = function(e) {
  message("Could not fetch ERDDAP time dimension: ", e$message)
  times <- NULL
})
#>  [1] "2026-01-28 12:00:00 UTC" "2026-01-31 12:00:00 UTC"
#>  [3] "2026-02-03 12:00:00 UTC" "2026-02-06 12:00:00 UTC"
#>  [5] "2026-02-09 12:00:00 UTC" "2026-02-12 12:00:00 UTC"
#>  [7] "2026-02-15 12:00:00 UTC" "2026-02-18 12:00:00 UTC"
#>  [9] "2026-02-21 12:00:00 UTC" "2026-02-24 12:00:00 UTC"
```

``` r
# let's limit to the most recent 10 times
(times <- tail(dims[["time"]], 10))
#>  [1] "2026-01-28 12:00:00 UTC" "2026-01-31 12:00:00 UTC"
#>  [3] "2026-02-03 12:00:00 UTC" "2026-02-06 12:00:00 UTC"
#>  [5] "2026-02-09 12:00:00 UTC" "2026-02-12 12:00:00 UTC"
#>  [7] "2026-02-15 12:00:00 UTC" "2026-02-18 12:00:00 UTC"
#>  [9] "2026-02-21 12:00:00 UTC" "2026-02-24 12:00:00 UTC"
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
#> Fetching request 1 of 1 (2026-01-28 to 2026-02-24) ~ 17:13:35 UTC
#> # A tibble: 10 × 4
#>    nms   lyr                      mean time               
#>    <chr> <chr>                   <dbl> <dttm>             
#>  1 FKNMS sss|2026-01-28 12:00:00  36.9 2026-01-28 12:00:00
#>  2 FKNMS sss|2026-01-31 12:00:00  34.5 2026-01-31 12:00:00
#>  3 FKNMS sss|2026-02-03 12:00:00  35.1 2026-02-03 12:00:00
#>  4 FKNMS sss|2026-02-06 12:00:00  38.8 2026-02-06 12:00:00
#>  5 FKNMS sss|2026-02-09 12:00:00  36.0 2026-02-09 12:00:00
#>  6 FKNMS sss|2026-02-12 12:00:00  35.1 2026-02-12 12:00:00
#>  7 FKNMS sss|2026-02-15 12:00:00  37.1 2026-02-15 12:00:00
#>  8 FKNMS sss|2026-02-18 12:00:00  35.3 2026-02-18 12:00:00
#>  9 FKNMS sss|2026-02-21 12:00:00  35.8 2026-02-21 12:00:00
#> 10 FKNMS sss|2026-02-24 12:00:00  37.0 2026-02-24 12:00:00
```

### Plot time series

``` r
d <- read_csv(d_csv)
head(d)
#> # A tibble: 6 × 4
#>   nms   lyr                      mean time               
#>   <chr> <chr>                   <dbl> <dttm>             
#> 1 FKNMS sss|2026-01-28 12:00:00  36.9 2026-01-28 12:00:00
#> 2 FKNMS sss|2026-01-31 12:00:00  34.5 2026-01-31 12:00:00
#> 3 FKNMS sss|2026-02-03 12:00:00  35.1 2026-02-03 12:00:00
#> 4 FKNMS sss|2026-02-06 12:00:00  38.8 2026-02-06 12:00:00
#> 5 FKNMS sss|2026-02-09 12:00:00  36.0 2026-02-09 12:00:00
#> 6 FKNMS sss|2026-02-12 12:00:00  35.1 2026-02-12 12:00:00

plot_ts(d, label = "Surface Salinity (PSU)")
```

### Map raster

``` r
r <- rast(r_tif)
names(r)
#>  [1] "sss|2026-01-28 12:00:00" "sss|2026-01-31 12:00:00"
#>  [3] "sss|2026-02-03 12:00:00" "sss|2026-02-06 12:00:00"
#>  [5] "sss|2026-02-09 12:00:00" "sss|2026-02-12 12:00:00"
#>  [7] "sss|2026-02-15 12:00:00" "sss|2026-02-18 12:00:00"
#>  [9] "sss|2026-02-21 12:00:00" "sss|2026-02-24 12:00:00"

lyr <- names(r)[1]
plet(r[lyr], tiles = "Esri.OceanBasemap")
#> Warning in colors(.): Some values were outside the color scale and will be
#> treated as NA
```
