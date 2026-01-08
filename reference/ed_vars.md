# Get variables from ERDDAP dataset

Given an ERDDAP dataset info object, return a character vector of all
available variables. Variables are values that vary along the dimensions
of the ERDDAP datasets (as returned by
[`ed_dims()`](https://marinebon.github.io/extractr/reference/ed_dims.md)).

## Usage

``` r
ed_vars(ed)
```

## Arguments

- ed:

  ERDDAP info object, as returned by
  [`ed_info`](https://marinebon.github.io/extractr/reference/ed_info.md))

## Value

character vector of variable names

## Examples

``` r
ed <- ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html")
ed_vars(ed)
#>      variable_name data_type actual_range
#> 1     analysed_sst    double             
#> 2   analysis_error    double             
#> 3             mask      byte             
#> 4 sea_ice_fraction    double             
```
