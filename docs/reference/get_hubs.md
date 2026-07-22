# Identify Stops Serving Multiple Routes

Identify Stops Serving Multiple Routes

## Usage

``` r
get_hubs(gtfs)
```

## Arguments

- gtfs:

  A GTFS object.

## Value

A WGS84 \`sf\` object with one row per served stop, list-columns
\`trip_id\` and \`route_id\`, counts \`n_trip\` and \`n_routes\`, and
point geometry.

## See also

\[GTFSwizard::plot_hubs()\], \[GTFSwizard::get_stops_sf()\]

## Examples

``` r
get_hubs(for_rail_gtfs)
#> Simple feature collection with 39 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -38.66031 ymin: -3.895013 xmax: -38.47702 ymax: -3.719291
#> Geodetic CRS:  WGS 84
#> # A tibble: 39 × 6
#>    stop_id trip_id     route_id  n_trip n_routes              geometry
#>    <chr>   <list>      <list>     <int>    <int>           <POINT [°]>
#>  1 28      <chr [185]> <chr [2]>    185        2 (-38.56387 -3.775791)
#>  2 27      <chr [127]> <chr [1]>    127        1 (-38.62042 -3.895013)
#>  3 26      <chr [127]> <chr [1]>    127        1  (-38.62687 -3.88752)
#>  4 25      <chr [127]> <chr [1]>    127        1 (-38.62556 -3.877935)
#>  5 24      <chr [127]> <chr [1]>    127        1 (-38.62003 -3.867487)
#>  6 23      <chr [127]> <chr [1]>    127        1 (-38.60866 -3.851034)
#>  7 22      <chr [127]> <chr [1]>    127        1 (-38.60069 -3.839444)
#>  8 21      <chr [127]> <chr [1]>    127        1 (-38.59204 -3.826729)
#>  9 20      <chr [127]> <chr [1]>    127        1 (-38.58571 -3.817101)
#> 10 19      <chr [127]> <chr [1]>    127        1 (-38.57773 -3.807445)
#> # ℹ 29 more rows
```
