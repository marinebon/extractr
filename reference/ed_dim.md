# Get dimension values from ERDDAP dataset

Given an ERDDAP dataset info object, return a vector of all available
values in given dimension.

## Usage

``` r
ed_dim(ed, dim)
```

## Arguments

- dim:

  dimension to extract

- ed_info:

  ERDDAP info object on SeaScape dataset, as returned by
  [`ed_info`](https://marinebon.github.io/extractr/reference/ed_info.md))

## Value

vector of values for given dimension

## Examples

``` r
ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")
ed_dim(ed, "LEV")
#> Warning: Failed to open 'https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.csvp?LEV': The requested URL returned error: 500
#> Error in open.connection(structure(4L, class = c("curl", "connection"), conn_id = <pointer: 0x55c05ea97930>),  : 
#>   cannot open the connection
#> Error in ed_dim(ed, "LEV"): Problem fetching dimension LEV from ERDDAP: https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.csvp?LEV
```
