# ERDDAP Extract

Extract data from ERDDAP dataset by bounding box and/or polygon. This
function provides the following enhancements to
[`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html):

1.  Throttle request to
    [`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html)
    by chunks of time, so the ERDDAP server does not become unresponsive
    with too big a request while minimizing the number of requests.

2.  Mask the grids by Area of Interest (`aoi`) (using
    [`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)).

3.  Summarize grids over time by `aoi`, including sub-geometries, into a
    CSV (using
    [`terra::zonal()`](https://rspatial.github.io/terra/reference/zonal.html)).

4.  Preserve intermediary NetCDF and/or GeoTIFF files for raster
    visualization.

## Usage

``` r
ed_extract(
  ed,
  var,
  sf_zones = NULL,
  fld_zones = NULL,
  bbox = NULL,
  zonal_csv = NULL,
  zonal_fun = "mean",
  rast_tif = NULL,
  mask_tif = TRUE,
  dir_nc = NULL,
  keep_nc = FALSE,
  n_max_vals_per_req = 1e+05,
  n_max_retries = 3,
  time_min = NULL,
  time_max = NULL,
  xy_tolerance = 1e-04,
  verbose = FALSE,
  ...
)
```

## Arguments

- ed:

  ERDDAP info object, of class
  [`rerddap::info`](https://docs.ropensci.org/rerddap/reference/info.html)
  as returned by
  [`ed_info`](https://marinebon.github.io/extractr/reference/ed_info.md))

- var:

  variable to extract

- sf_zones:

  spatial feature object
  ([`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html)) with
  zones to extract zonal statistics from ERDDAP gridded data. If
  `sf_zones` is left to the default `NULL` value, then the `bbox` is
  used.

- fld_zones:

  character vector of unique field name(s) in `sf_zones` to include in
  extracted zonal statistics

- bbox:

  bounding box (e.g.,
  `c(xmin = -83.0, ymin = 27.3, xmax = -81.8, ymax= 28.5)`), used to
  extract the grid (i.e., as `longitude` and `latitude` arguments to
  [`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html)).
  If `bbox` is left to the default `NULL` value, then the bounding box
  is derived from the bounding box of the `sf_zones`.

- zonal_csv:

  output zonal statistics as CSV from `terra::zonal(rast_tif, aoi)`

- zonal_fun:

  function to summarize the data over time by `sf_zones`. Default:

- rast_tif:

  optional output as GeoTIFF masked to `sf_zones` from NetCDF

- mask_tif:

  mask the GeoTIFF by `sf_zones`. Default: TRUE

- dir_nc:

  optional output directory to keep NetCDF files written to disk by
  [`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html)

- keep_nc:

  keep NetCDF files written to disk by
  [`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html)
  server at a time. Default: 100,000

- n_max_vals_per_req:

  maximum number of values to request from ERDDAP

- n_max_retries:

  maximum number of retries to request data from ERDDAP server

- time_min:

  minimum time to extract from ERDDAP dataset

- time_max:

  maximum time to extract from ERDDAP dataset

- xy_tolerance:

  tolerance to use when aligning raster and vector data. Default: 0.0001

- verbose:

  display messages on status of function. Useful for debugging or
  showing status while getting data from a wide range and/or big
  polygon. Default: FALSE.

- ...:

  arguments to pass along to
  [`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html),
  such as to filter the request by dimensions

## Value

data frame of the zonal summary of the raster (optionally also written
to zonal_csv)

## Examples

``` r
if (FALSE) { # \dontrun{
ed <- ed_info("https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html")
(vars <- ed_vars(ed))
dims <- ed_dims(ed)
times = tail(dims$time, 10)
ed_extract(
  ed,
  "analysed_sst",
  bbox = c(xmin = -83.0, ymin = 27.3, xmax = -81.8, ymax= 28.5),
  time_min = min(times),
  time_max = max(times) )
} # }
```
