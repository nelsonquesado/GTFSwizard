# Calculate Scheduled Speeds

Combines distance in meters and duration in seconds to calculate
kilometers per hour.

## Usage

``` r
get_speeds(gtfs, method = "by_trip", trips = "all")
```

## Arguments

- gtfs:

  A GTFS object.

- method:

  One of \`"by_trip"\`, \`"by_route"\`, or \`"detailed"\`. Legacy dotted
  values remain accepted.

- trips:

  Character trip IDs or \`"all"\`.

## Value

A tibble with \`average.speed\` for route and trip methods, or
segment-level \`speed\` for the detailed method. \`direction_id\` is
retained when available.

## Details

Detailed distances are straight-line geodesic distances between stops.
Route and trip distances follow \`shapes.txt\`, or inferred
straight-line shapes if the feed has none.

## See also

\[GTFSwizard::get_distances()\], \[GTFSwizard::get_durations()\]

## Examples

``` r
get_speeds(for_rail_gtfs, "by_route")
#> # A tibble: 6 × 6
#>   route_id direction_id trips average.speed service_pattern  pattern_frequency
#>   <chr>           <int> <int>         <dbl> <chr>                        <int>
#> 1 6                   0    63          38.3 servicepattern-1               614
#> 2 6                   1    64          38.3 servicepattern-1               614
#> 3 7                   0    15          26.3 servicepattern-1               614
#> 4 7                   1    15          25.7 servicepattern-1               614
#> 5 8                   0    29          21.9 servicepattern-1               614
#> 6 8                   1    29          21.9 servicepattern-1               614
get_speeds(for_rail_gtfs, "by_trip")
#> # A tibble: 215 × 6
#>    route_id trip_id direction_id average.speed service_pattern pattern_frequency
#>    <chr>    <chr>          <int>         <dbl> <chr>                       <int>
#>  1 7        10                 0          26.3 servicepattern…               614
#>  2 6        100                1          38.3 servicepattern…               614
#>  3 6        101                1          38.3 servicepattern…               614
#>  4 6        102                1          38.3 servicepattern…               614
#>  5 6        103                1          38.3 servicepattern…               614
#>  6 6        104                1          38.3 servicepattern…               614
#>  7 6        105                1          38.3 servicepattern…               614
#>  8 6        106                1          38.3 servicepattern…               614
#>  9 6        107                1          38.3 servicepattern…               614
#> 10 6        108                1          38.3 servicepattern…               614
#> # ℹ 205 more rows
```
