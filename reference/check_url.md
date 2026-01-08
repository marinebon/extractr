# Check if a URL is online

Check if a URL is online by making a HEAD request. Returns TRUE if the
URL is reachable and the status code is between 200 and 399 (inclusive).
Returns FALSE otherwise.

## Usage

``` r
check_url(url, timeout_ms = 5000)
```

## Arguments

- url:

  link to check

- timeout_ms:

  timeout in milliseconds

## Value

list with online status, status code, and message

## Details

This function is useful for checking the availability of a server or
resource before attempting to access it. In particular, it is used by
[`ed_info()`](https://marinebon.github.io/extractr/reference/ed_info.md)
to check if the ERDDAP server is online before making requests to it
since the underlying
[`rerddap::info()`](https://docs.ropensci.org/rerddap/reference/info.html)
crashes R if `url` is offline.

## Examples

``` r
check_url("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html")
#> $online
#> [1] FALSE
#> 
#> $status_code
#> [1] 503
#> 
#> $message
#> [1] "Server is online with status code: 503"
#> 
```
