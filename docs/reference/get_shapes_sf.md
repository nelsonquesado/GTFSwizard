# Convert GTFS Shapes to Simple Features

Convert GTFS Shapes to Simple Features

## Usage

``` r
get_shapes_sf(gtfs)
```

## Arguments

- gtfs:

  A GTFS object or a \`shapes\` data frame.

## Value

For a data frame, one WGS84 \`LINESTRING\` feature per \`shape_id\`. For
a GTFS object, the same object with its \`shapes\` table replaced by
that \`sf\` object. If present, \`shape_dist_traveled\` is summarized as
its maximum value for each shape.

## See also

\[GTFSwizard::get_shapes_df()\], \[GTFSwizard::get_shapes()\]

## Examples

``` r
gtfs_sf <- get_shapes_sf(for_rail_gtfs)
shapes_sf <- get_shapes_sf(for_rail_gtfs$shapes)
```
