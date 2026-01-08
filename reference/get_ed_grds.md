# Get Seascape grids within polygon for date range

Given a polygon and date range, fetch Seascape data and return a raster
layer for a single date or raster stack if multiple dates found for
given date range.

## Usage

``` r
get_ed_grds(
  ed_info,
  ply,
  ed_var = "CLASS",
  date_beg = min(ed_dim(ed_info, "time")),
  date_end = max(ed_dim(ed_info, "time")),
  dir_tif = NULL,
  del_cache = F,
  verbose = T
)
```

## Arguments

- ed_info:

  SeaScape ERDDAP info object, as returned by
  [`ed_info`](https://marinebon.github.io/extractr/reference/ed_info.md))

- ply:

  polygon as spatial feature
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html), as returned
  by
  [`get_url_ply`](https://marinebon.github.io/extractr/reference/get_url_ply.md)
  or `bbox_to_ply`

- ed_var:

  SeaScape variable. One of "CLASS" (default) or "P" for probability.

- date_beg:

  date begin to fetch, as character (`"2003-01-15"`) or Date
  (`Date("2003-01-15")`). Defaults to first date available from
  `ed_info`.

- date_end:

  date end to fetch, as character (`"2020-11-15"`) or Date
  (`Date("2020-11-15")`). Defaults to latest date available from
  `ed_info`.

- dir_tif:

  directory to cache results. Files are stored in the format
  `grd_{ed_var}_{date}.tif` so any other information needed, such as
  place or SeaScape dataset should be captured by the containing folders
  so as to not inadvertently write or read the wrong grid. This folder
  is consulted for available dates before fetching any missing from the
  ERDDAP server.

- del_cache:

  Delete ERDDAP cache with
  [`cache_delete_all`](https://docs.ropensci.org/rerddap/reference/cache_delete.html),
  which may be necessary if underlying data changed. Default: FALSE.

- verbose:

  display messages on status of function. Useful for debugging or
  showing status while getting data from a wide range and/or big
  polygon. Default: FALSE.

## Value

Raster [`raster`](https://rdrr.io/pkg/raster/man/raster.html) layer if
one date, [`stack`](https://rdrr.io/pkg/raster/man/stack.html) if more

## Examples

``` r
ply  <- get_url_ply("mbnms")
#> Error in dir_create(dir_ply): could not find function "dir_create"
ed_i <- ed_info()
#> Error in ed_info(): argument "dataset" is missing, with no default
grds <- get_ed_grds(ed_i, ply, date_beg = "2020-01-01")
#> Error: object 'ed_i' not found
grds
#> Error: object 'grds' not found
```
