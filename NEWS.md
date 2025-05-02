# extractr 0.1.4

* Added new function `check_url()` to `ed_info()` to check if the ERDDAP server 
  is valid and working, since running `rerddap::info(dataset, url = ed_url)` on
  offline ERDDAP crashes R.
* Fixed #8 with check for extra dimensions.

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
