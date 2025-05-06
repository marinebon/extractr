# extractr 0.1.5

* Added params to `ed_extract()` documentation: `keep_nc`, `n_max_vals_per_req`, 
 `n_max_retries`, `time_min`, `time_max`, `verbose`.
 
* The `ed_extract()` function now returns invisible first argument `ed`, per 
  [6 Functions | Advanced R](https://adv-r.hadley.nz/functions.html#invisible).

* Updated pkgdown to bootstrap 5 with Reference sections.

* Fix removal of temporary `*_nc` folders from `ed_extract()` if `keep_nc = FALSE`.

# extractr 0.1.4

* Added new function `check_url()` to `ed_info()` to check if the ERDDAP server 
  is valid and working, since running `rerddap::info(dataset, url = ed_url)` on
  offline ERDDAP crashes R.
  
* Fixed #8 with check for extra dimensions.

* Updated vignette to use datasets:

  - [ERDDAP - Sea Surface Temperature, NOAA Coral Reef Watch Daily Global 5km Satellite SST (CoralTemp), 1985-present, Daily - Data Access Form](https://coastwatch.noaa.gov/erddap/griddap/noaacrwsstDaily.html)
  - [ERDDAP - Sea Surface Salinity, Miras SMOS, Near Real-Time, Global 0.25°, 2010-present, 3 Day Composite - Data Access Form](https://coastwatch.noaa.gov/erddap/griddap/noaacwSMOSsss3day.html)

# extractr 0.1.3

* Modified `plot_ts()` so first argument could either be path to CSV or data frame.

# extractr 0.1.2

* Fixed arguments in `rerddap::griddap()` for rerddap version `1.0.3`:  `x` to `datasetx`;  `time` with `as.character()`. Fixed article outputs.

# extractr 0.1.1

* Fix `get_ed_raster()`, `grds_to_ts()` so working with [noaa-onms/climate-dashboard](https://github.com/noaa-onms/climate-dashboard) ed_var = "CRW_SST" coral reef watch 

# extractr 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Added `grds_to_ts()` for continuous, `grds_to_ts_cat()` for categorical (from [seascapeR](https://marinebon.github.io/seascapeR/))

* Cleaned up `get_ed_grds()`

* Added `plot_ts()` to plot timeseries
