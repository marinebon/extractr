# Get ERDDAP dataset information

Get ERDDAP dataset information.

## Usage

``` r
ed_info(dataset)
```

## Arguments

- dataset:

  `{region}_{frequency}` of dataset. Valid values (so far):
  "global_8day" or "global_monthly" (default).

## Value

ERDDAP [`info`](https://docs.ropensci.org/rerddap/reference/info.html)
object

## Examples

``` r
ed_info() # default: dataset = "global_monthly"
#> Error in ed_info(): argument "dataset" is missing, with no default
ed_info("global_8day")
#> Error in path_to_connection(x): 'Error { code=404; message="Not Found: Currently unknown
#> datasetID=global_8day"; } ' does not exist in current working directory
#> (/home/runner/work/extractr/extractr/docs/reference).
```
