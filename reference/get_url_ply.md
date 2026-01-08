# Get polygon from link or NOAA Sanctuary code

Given a sanctuary code or link (ie URL), download the zip, unzip it, and
read first shapefile as a spatial feature. This function was originally
designed to pull from one of the zip links found at [NOAA Sanctuaries
GIS](https://sanctuaries.noaa.gov/library/imast_gis.html).

## Usage

``` r
get_url_ply(
  sanctuary = NULL,
  url = NULL,
  dir_ply = here::here("data_ed/ply"),
  verbose = F
)
```

## Arguments

- sanctuary:

  NOAA Sanctuary code with which to form `url`. Sanctuary codes can be
  found at [NOAA Sanctuaries
  GIS](https://sanctuaries.noaa.gov/library/imast_gis.html).

- url:

  Link from which to fetch a polygon. Originally designed to pull from
  one of the zip links found at [NOAA Sanctuaries
  GIS](https://sanctuaries.noaa.gov/library/imast_gis.html).

- dir_ply:

  Directory to download locally into. This directory serves as a cache
  to skip operations if already performed. If zip file exists, skip
  downloading. If directory to unzip exists, skip unzipping.

- verbose:

  Verbose meedages describing operations and choices, such as skipping
  download or unzip and which shapefile used if more than one found.

## Value

Returns a spatial feature
[sf](https://r-spatial.github.io/sf/reference/sf.html) polygon data
frame.

## Examples

``` r
# mbnms: Monterey Bay National Marine Sanctuary
ply_mbnms <- get_url_ply(sanctuary = "mbnms")
#> Error in dir_create(dir_ply): could not find function "dir_create"
ply_mbnms
#> Error: object 'ply_mbnms' not found
plot(ply_mbnms[1])
#> Error: object 'ply_mbnms' not found

# fknms: Florida Keys National Marine Sanctuary
ply_fknms <- get_url_ply(sanctuary = "fknms")
#> Error in dir_create(dir_ply): could not find function "dir_create"
ply_fknms
#> Error: object 'ply_fknms' not found
plot(ply_fknms[1])
#> Error: object 'ply_fknms' not found
```
