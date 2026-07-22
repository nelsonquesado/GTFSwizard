# Infer Straight-Line Shapes from Stop Sequences

Builds one shape for each unique ordered stop pattern and assigns it to
the corresponding trips.

## Usage

``` r
get_shapes(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A \`wizardgtfs\` object with a new \`shapes\` table and updated
\`trips\$shape_id\`.

## Details

Consecutive stop coordinates are connected directly. The result is
useful for visualization and approximate distance analysis, but it is
not a map-matched representation of the vehicle path.

## See also

\[GTFSwizard::get_shapes_sf()\], \[GTFSwizard::get_shapes_df()\]

## Examples

``` r
gtfs_with_shapes <- get_shapes(for_rail_gtfs)
#> Warning: GTFSwizard: overwriting the existing `shapes` table with inferred shapes.
```
