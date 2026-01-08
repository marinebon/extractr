# Plot time series

The purpose of this function is to generate time series plots of ERRDAP
data.

## Usage

``` r
plot_ts(
  ts,
  fld_avg = "mean",
  fld_sd = NULL,
  fld_date = "time",
  color = "red",
  label = "Temperature (°C)",
  ...
)
```

## Arguments

- ts:

  file path to timeseries table as comma-separated value (CSV) or data
  frame (required)

- fld_avg:

  field name containing value average; default = `"mean"`

- fld_sd:

  field name containing standard deviation average (optional); default =
  `NULL`

- fld_date:

  field name containing date (required); default = `"time"`

- color:

  color to plot value; default = `"red"`

- label:

  label for y-axes of plot

- ...:

  additional parameters to pass to
  [`dygraphs::dygraph()`](https://rdrr.io/pkg/dygraphs/man/dygraph.html)

## Value

[`dygraphs::dygraph()`](https://rdrr.io/pkg/dygraphs/man/dygraph.html)
object of the time series plot

## Examples

``` r
if (FALSE) { # \dontrun{
ts <- here::here("data_tmp/ts.csv")
plot_ts(ts, main = "SST")
} # }
```
