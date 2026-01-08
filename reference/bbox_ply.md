# Create polygon from bounding box

Utility function to create polygon from bounding box

## Usage

``` r
bbox_ply(lon_min, lat_min, lon_max, lat_max)
```

## Arguments

- lon_min:

  longitude, minimum

- lat_min:

  latitude, minimum

- lon_max:

  longitude, maximum

- lat_max:

  latitude, maximum

## Value

Returns a spatial feature
[sf](https://r-spatial.github.io/sf/reference/sf.html) polygon.

## Examples

``` r
# Florida Keys area
lon = -81.3; lat = 24.5; w = 10
bbox_ply(lon - w, lat - w, lon + w, lat + w)
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -91.3 ymin: 14.5 xmax: -71.3 ymax: 34.5
#> CRS:           NA
#> POLYGON ((-91.3 14.5, -71.3 14.5, -71.3 34.5, -...
```
