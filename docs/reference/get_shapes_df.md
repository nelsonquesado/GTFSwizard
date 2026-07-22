# Convert Shape Geometries to a GTFS Shapes Table

Convert Shape Geometries to a GTFS Shapes Table

## Usage

``` r
get_shapes_df(shape)
```

## Arguments

- shape:

  An \`sf\` object with a \`shape_id\` field and line geometries.

## Value

A tibble containing \`shape_id\`, WGS84 point coordinates,
\`shape_pt_sequence\`, and cumulative \`shape_dist_traveled\` in meters.

## Details

Distances are geodesic approximations between consecutive coordinates.
GTFS permits any consistent distance unit; this function uses meters.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/#shapestxt)

## See also

\[GTFSwizard::get_shapes_sf()\], \[GTFSwizard::get_shapes()\]

## Examples

``` r
shapes_sf <- get_shapes_sf(for_rail_gtfs)$shapes
shapes_df <- get_shapes_df(shapes_sf)
```
