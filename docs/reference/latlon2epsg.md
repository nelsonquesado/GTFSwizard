# Transform Spatial Data to a Local Metric CRS

Selects a UTM zone from the geographic centroid, or a polar
stereographic CRS outside UTM's latitude range.

## Usage

``` r
latlon2epsg(sf_obj)
```

## Arguments

- sf_obj:

  An \`sf\` or \`sfc\` object with a defined CRS.

## Value

The input transformed to EPSG 326xx/327xx, EPSG 3413, or EPSG 3031.

## See also

\[sf::st_transform()\]

## Examples

``` r
shapes <- get_shapes_sf(for_rail_gtfs)$shapes
metric_shapes <- latlon2epsg(shapes)
```
