# Summarize categorical grids into time series table

Summarize grids into a table having columns: `date`, `cellvalue` (e.g.
Seascape CLASS) and number of cells (`n_cells`).

## Usage

``` r
grds_to_ts_cat(grds, ts_csv = NULL, verbose = F)
```

## Arguments

- grds:

  raster stack with more than one date, as returned by `get_ss_grds`

- ts_csv:

  path to csv to save this time series table. Default is NULL, in which
  case the table is not saved. If path is set and already exists then
  that will be read in if all dates in the `grds` are present instead of
  recalculating and writing to `ts_csv`.

- verbose:

  show messages of process. Useful for debugging. Default: FALSE.

## Value

`tibble` of data

## Details

This function is particularly helpful in between using
[`get_ed_grds()`](https://marinebon.github.io/extractr/reference/get_ed_grds.md)
and `plot_ts_cat()`.

## Examples

``` r
ply  <- get_url_ply("mbnms")
#> Error in dir_create(dir_ply): could not find function "dir_create"
ss_i <- get_ss_info()
#> Error in get_ss_info(): could not find function "get_ss_info"
grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#> Error in get_ss_grds(ss_i, ply, date_beg = "2020-01-01"): could not find function "get_ss_grds"
tbl  <- sum_ss_grds_to_ts(grds)
#> Error in sum_ss_grds_to_ts(grds): could not find function "sum_ss_grds_to_ts"
tbl
#> Error: object 'tbl' not found
```
