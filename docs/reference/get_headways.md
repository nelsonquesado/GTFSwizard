# Calculate Scheduled Headways

Calculates elapsed minutes between successive service instances.
Frequency based trips are expanded according to \`frequencies.txt\`.

## Usage

``` r
get_headways(gtfs, method = "by_trip")
```

## Arguments

- gtfs:

  A GTFS object.

- method:

  One of \`"by_trip"\`, \`"by_route"\`, \`"by_hour"\`, \`"by_stop"\`,
  \`"by_shape"\`, or \`"detailed"\`. Legacy dotted values remain
  accepted.

## Value

A tibble. Headway values are always returned in \`headway_minutes\`;
\`valid_trips\` is the number of intervals summarized.

## Details

Route, hour, and trip methods compare first departures. Stop and
detailed methods compare arrivals at the same stop, route, direction,
and service pattern. The first service instance in each group has no
headway and is omitted.

## References

\[GTFS Schedule
Reference\](https://gtfs.org/documentation/schedule/reference/#frequenciestxt)

## See also

\[GTFSwizard::get_frequency()\]

## Examples

``` r
get_headways(for_rail_gtfs, "by_route")
#> # A tibble: 6 × 6
#>   route_id direction_id service_pattern  pattern_frequency headway_minutes
#>   <chr>           <int> <chr>                        <int>           <dbl>
#> 1 6                   0 servicepattern-1               614            17.0
#> 2 6                   1 servicepattern-1               614            16.8
#> 3 7                   0 servicepattern-1               614            62.1
#> 4 7                   1 servicepattern-1               614            62.1
#> 5 8                   0 servicepattern-1               614            37  
#> 6 8                   1 servicepattern-1               614            37  
#> # ℹ 1 more variable: valid_trips <int>
get_headways(for_rail_gtfs, "by_hour")
#> # A tibble: 19 × 5
#>     hour service_pattern  pattern_frequency headway_minutes valid_trips
#>    <dbl> <chr>                        <int>           <dbl>       <int>
#>  1     5 servicepattern-1               614            16             2
#>  2     6 servicepattern-1               614            26.1          14
#>  3     7 servicepattern-1               614            29.3          15
#>  4     8 servicepattern-1               614            24.3          12
#>  5     9 servicepattern-1               614            26.7          12
#>  6    10 servicepattern-1               614            27.9          11
#>  7    11 servicepattern-1               614            33.8          13
#>  8    12 servicepattern-1               614            28.2          13
#>  9    13 servicepattern-1               614            29.1          10
#> 10    14 servicepattern-1               614            29.3          13
#> 11    15 servicepattern-1               614            29.2          12
#> 12    16 servicepattern-1               614            32.6          14
#> 13    17 servicepattern-1               614            26.9          13
#> 14    18 servicepattern-1               614            22.5          13
#> 15    19 servicepattern-1               614            29.2          13
#> 16    20 servicepattern-1               614            26.1          14
#> 17    21 servicepattern-1               614            28.5           6
#> 18    22 servicepattern-1               614            32.7           7
#> 19    23 servicepattern-1               614            47             2
```
